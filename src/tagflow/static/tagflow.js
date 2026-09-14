/**
 * Tagflow live client. `<tagflow-client session-id="…">` connects to the
 * server's session and applies the updates it sends: each update is a list
 * of morphs, and each morph is the new outer HTML of one element found by
 * id. The element is morphed in place with Idiomorph (loaded separately as
 * `window.Idiomorph`), the same algorithm htmx uses, so focus, scroll
 * position, and unchanged nodes survive.
 *
 * Events, dispatched on the element with the `tagflow:` prefix:
 *   connected, disconnected, update ({morphs}), morph ({target, ok, error}),
 *   expired (cancelable; the default action reloads the page).
 */
class TagflowClient extends HTMLElement {
  constructor() {
    super();
    this.socket = null;
    this.retryTimer = null;
    this.retryDelay = 1000;
  }

  connectedCallback() {
    this.connect();
  }

  disconnectedCallback() {
    this.close();
  }

  get sessionId() {
    return this.getAttribute("session-id");
  }

  get socketUrl() {
    const protocol = location.protocol === "https:" ? "wss:" : "ws:";
    return `${protocol}//${location.host}/.well-known/tagflow/live.ws`;
  }

  emit(name, detail, options = {}) {
    const event = new CustomEvent(`tagflow:${name}`, { detail, ...options });
    return this.dispatchEvent(event);
  }

  connect() {
    this.close();
    const socket = new WebSocket(this.socketUrl);
    this.socket = socket;
    socket.onopen = () => {
      this.retryDelay = 1000;
      socket.send(JSON.stringify({ id: this.sessionId }));
      this.emit("connected");
    };
    socket.onmessage = (event) => this.receive(JSON.parse(event.data));
    socket.onclose = (event) => {
      if (socket !== this.socket) return; // superseded by a newer connection
      this.socket = null;
      this.emit("disconnected", { code: event.code });
      if (event.code === 4001) {
        this.expire();
      } else {
        this.retryTimer = setTimeout(() => this.connect(), this.retryDelay);
        this.retryDelay = Math.min(this.retryDelay * 2, 15000);
      }
    };
  }

  close() {
    clearTimeout(this.retryTimer);
    this.retryTimer = null;
    if (this.socket) {
      const socket = this.socket;
      this.socket = null;
      socket.close();
    }
  }

  /** The server no longer knows this session, so this page is stale. */
  expire() {
    if (this.emit("expired", null, { cancelable: true })) {
      location.reload();
    }
  }

  receive(message) {
    if (message.type !== "update") return;
    const apply = () => this.applyUpdate(message.morphs);
    if (document.startViewTransition) {
      document.startViewTransition(apply);
    } else {
      apply();
    }
  }

  applyUpdate(morphs) {
    for (const morph of morphs) {
      const target = document.getElementById(morph.target);
      if (!target) {
        const error = new Error(`No element with id ${morph.target}`);
        this.emit("morph", { target: morph.target, ok: false, error });
        continue;
      }
      try {
        Idiomorph.morph(target, morph.html, { morphStyle: "outerHTML" });
        this.emit("morph", { target: morph.target, ok: true });
      } catch (error) {
        this.emit("morph", { target: morph.target, ok: false, error });
      }
    }
    this.emit("update", { morphs });
  }
}

customElements.define("tagflow-client", TagflowClient);
