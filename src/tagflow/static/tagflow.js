/**
 * Tagflow client-side implementation for handling live DOM updates
 * via WebSocket connections.
 */
class TagflowClient extends HTMLElement {
  // Define observed attributes
  static get observedAttributes() {
    return ["session-id"];
  }

  constructor() {
    super();
    this.socket = null;
    this.connected = false;
    this.reconnectTimer = null;

    // Bind methods to preserve 'this' context
    this.handleMessage = this.handleMessage.bind(this);
    this.handleOpen = this.handleOpen.bind(this);
    this.handleClose = this.handleClose.bind(this);
    this.handleError = this.handleError.bind(this);
  }

  /**
   * Called when the element is added to the document
   */
  connectedCallback() {
    if (!this.hasAttribute("session-id")) {
      this.setAttribute("session-id", crypto.randomUUID());
    }
    this.connect();
  }

  /**
   * Called when the element is removed from the document
   */
  disconnectedCallback() {
    this.disconnect();
  }

  /**
   * Called when attributes change
   */
  attributeChangedCallback(name, oldValue, newValue) {
    if (name === "session-id" && oldValue !== newValue && this.connected) {
      this.reconnect();
    }
  }

  /**
   * Get the current session ID
   */
  get sessionId() {
    return this.getAttribute("session-id");
  }

  /**
   * Get the WebSocket URL based on current protocol
   */
  get wsUrl() {
    const protocol = window.location.protocol === "https:" ? "wss:" : "ws:";
    return `${protocol}//${window.location.host}/.well-known/tagflow/live.ws`;
  }

  /**
   * Emit a custom event
   */
  emit(name, detail = null) {
    const event = new CustomEvent(
      `tagflow:${name}`,
      detail ? { detail } : undefined
    );
    this.dispatchEvent(event);
  }

  /**
   * Find a target element by ID and throw if not found
   */
  findTarget(id, context = "element") {
    const element = document.getElementById(id);
    if (!element) {
      throw new Error(`Target ${context} not found: ${id}`);
    }
    return element;
  }

  /**
   * Connect to the Tagflow WebSocket endpoint
   */
  connect() {
    if (this.socket) {
      this.disconnect();
    }

    this.socket = new WebSocket(this.wsUrl);
    this.socket.onopen = this.handleOpen;
    this.socket.onmessage = this.handleMessage;
    this.socket.onclose = this.handleClose;
    this.socket.onerror = this.handleError;
  }

  /**
   * Reconnect to the WebSocket server
   */
  reconnect() {
    this.disconnect();
    this.connect();
  }

  /**
   * Disconnect from the WebSocket server
   */
  disconnect() {
    if (this.reconnectTimer) {
      clearTimeout(this.reconnectTimer);
      this.reconnectTimer = null;
    }

    if (this.socket) {
      this.socket.close();
      this.socket = null;
    }
    this.connected = false;
  }

  /**
   * WebSocket event handlers
   */
  handleOpen() {
    this.connected = true;
    this.emit("connected");
    this.socket.send(JSON.stringify({ id: this.sessionId }));
  }

  handleMessage(event) {
    const message = JSON.parse(event.data);
    if (message.type === "update") {
      if (false && document.startViewTransition) {
        document.startViewTransition(() => {
          this.applyMutations(message.mutations);
        });
      } else {
        this.applyMutations(message.mutations);
      }
    }
  }

  handleClose() {
    this.connected = false;
    this.emit("disconnected");
    this.reconnectTimer = setTimeout(() => this.connect(), 1000);
  }

  handleError(error) {
    this.emit("error", error);
  }

  /**
   * Apply a list of mutations to the DOM
   */
  applyMutations(mutations) {
    const handlers = {
      openTag: this.handleOpenTag.bind(this),
      closeTag: this.handleCloseTag.bind(this),
      setAttribute: this.handleSetAttribute.bind(this),
      setText: this.handleSetText.bind(this),
      clear: this.handleClear.bind(this),
    };

    for (const mutation of mutations) {
      try {
        const handler = handlers[mutation.type];
        if (handler) {
          handler(mutation);
          this.emit("mutation", { mutation, success: true });
        }
      } catch (error) {
        this.emit("mutation", { mutation, success: false, error });
      }
    }
  }

  /**
   * Mutation handlers
   */
  handleOpenTag({ target, id, tag, attrs = {} }) {
    const parent = this.findTarget(target, "parent");
    const element = document.createElement(tag);
    element.id = id;

    // Apply initial attributes
    Object.entries(attrs).forEach(([name, value]) => {
      element.setAttribute(name, value);
    });

    parent.appendChild(element);
  }

  handleCloseTag({ target }) {
    // We don't actually need to do anything here in the DOM
    // since the element is already in the right place.
    // This event is more for tracking the structure.
    this.findTarget(target);
  }

  handleSetAttribute({ target, name, value }) {
    const element = this.findTarget(target);
    element.setAttribute(name, value);
  }

  handleSetText({ target, value }) {
    const element = this.findTarget(target);
    element.textContent = value;
  }

  handleClear({ target }) {
    const element = this.findTarget(target);
    while (element.firstChild) {
      element.removeChild(element.firstChild);
    }
  }
}

// Register the custom element
customElements.define("tagflow-client", TagflowClient);
