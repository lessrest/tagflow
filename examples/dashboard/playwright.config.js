const { defineConfig } = require("@playwright/test");

module.exports = defineConfig({
  testDir: "./browser-tests",
  workers: 1,
  use: { baseURL: "http://127.0.0.1:8011" },
  webServer: {
    command:
      "uv run --no-sync hypercorn test.dashboard_server:app --bind 127.0.0.1:8011",
    cwd: "../..",
    url: "http://127.0.0.1:8011/campaigns/native",
    reuseExistingServer: false,
  },
});
