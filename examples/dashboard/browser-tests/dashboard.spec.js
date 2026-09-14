const { test, expect } = require("@playwright/test");

for (const failure of ["network", "server"]) {
  test(`following retries ${failure} failures, tolerates slow responses, and stops at EOF`, async ({
    page,
  }) => {
    let attempts = 0;
    await page.route("**/builds/1/log?*", async (route) => {
      attempts++;
      if (attempts === 1) {
        return failure === "network"
          ? route.abort("failed")
          : route.fulfill({ status: 503, body: "Temporary failure" });
      }
      // Longer than the poll interval: a new tick must not abort this request.
      await new Promise((resolve) => setTimeout(resolve, 3000));
      await route.continue();
    });
    await page.goto("/builds/1?transport=poll");
    await expect(page.locator(".log-line")).toHaveCount(9, { timeout: 15000 });
    await expect(page.locator(".log-tail")).toHaveCount(0);
    await expect(page.locator("#build-log")).toContainText("End of build log");
    const finishedAttempts = attempts;
    await page.waitForTimeout(4500);
    expect(attempts).toBe(finishedAttempts);
    expect(attempts).toBe(2);
    const ids = await page
      .locator(".log-line")
      .evaluateAll((lines) => lines.map((line) => line.id));
    expect(new Set(ids).size).toBe(9);
  });
}

for (const action of ["select", "pause", "navigate"]) {
  test(`late reset cannot overwrite a replacement after ${action}`, async ({
    page,
  }) => {
    let release;
    const held = new Promise((resolve) => {
      release = resolve;
    });
    let started;
    const pending = new Promise((resolve) => {
      started = resolve;
    });
    let delivered;
    const responseDelivered = new Promise((resolve) => {
      delivered = resolve;
    });
    await page.route("**/builds/3/log?*", async (route) => {
      const url = new URL(route.request().url());
      url.searchParams.set("epoch", "old-process");
      const response = await route.fetch({ url: url.toString() });
      started();
      await held;
      await route.fulfill({ response });
      delivered();
    });
    await page.goto("/campaigns/native?transport=poll");
    await pending;
    if (action === "select") {
      await page.getByRole("link", { name: "sqlite", exact: true }).click();
      await expect(page.locator("#build-detail h2")).toHaveText("sqlite");
    } else if (action === "pause") {
      await page
        .getByRole("link", { name: "Pause following", exact: true })
        .click();
      await expect(
        page.getByRole("link", { name: "Resume following", exact: true }),
      ).toBeVisible();
    } else {
      await page.getByRole("link", { name: "Next →", exact: true }).click();
      await expect(page).toHaveURL(/page=2/);
    }
    const replacement = await page.locator("#build-detail").elementHandle();
    release();
    await responseDelivered;
    // Allow response parsing/swapping to complete, then assert node identity.
    await page.waitForTimeout(500);
    expect(
      await replacement.evaluate(
        (node) => node === document.querySelector("#build-detail"),
      ),
    ).toBe(true);
    await expect(page).toHaveTitle("Native build campaign · Tagflow");
  });
}

test("reset preserves polling and preview title; standalone SSE connects", async ({
  page,
}) => {
  const eventRequest = page.waitForRequest("**/campaigns/native/events");
  await page.goto("/builds/3");
  await eventRequest;
  await expect(page.locator("#changes")).toHaveAttribute(
    "hx-sse:connect",
    "/campaigns/native/events",
  );
  await page.goto("/campaigns/native?transport=poll");
  await page.route("**/builds/3/log?*", async (route) => {
    const url = new URL(route.request().url());
    url.searchParams.set("epoch", "old-process");
    const response = await route.fetch({ url: url.toString() });
    await route.fulfill({ response });
  });
  const old = await page.locator("#build-detail").elementHandle();
  await expect.poll(() => old.evaluate((node) => node.isConnected)).toBe(false);
  await expect(page.locator("#build-status")).toHaveAttribute(
    "hx-trigger",
    "every 3s",
  );
  await expect(page.locator(".log-tail")).toHaveAttribute(
    "href",
    /transport=poll/,
  );
  await expect(page).toHaveTitle("Native build campaign · Tagflow");
});

test("log pages are navigable without JavaScript", async ({ browser }) => {
  const context = await browser.newContext({ javaScriptEnabled: false });
  const page = await context.newPage();
  await page.goto("http://127.0.0.1:8011/builds/1/log?transport=poll");
  await expect(page.getByRole("heading", { level: 1 })).toHaveText(
    "zlib: build output",
  );
  await page.getByRole("link", { name: "Read next lines →" }).click();
  await expect(page.locator(".log-line")).toHaveCount(4);
  await page.getByRole("link", { name: "← zlib" }).click();
  await expect(page).toHaveURL(/\/builds\/1\?/);
  await context.close();
});
