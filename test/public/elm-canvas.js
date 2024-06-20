// The version adapted for gradients at: https://package.elm-lang.org/packages/shamansir/elm-canvas
// Original version is at: https://package.elm-lang.org/packages/joakin/elm-canvas/latest/
const { drawText } = window.canvasTxt
// Only run the script on the browser
if (typeof window !== "undefined") {
  if (window["customElements"]) {
    CanvasRenderingContext2D.prototype.drawText = function(text, settings) {
        drawText(this, text, settings);
    }
    customElements.define(
      "elm-canvas",
      class extends HTMLElement {
        constructor() {
          super();
          this.commands = [];
          this.mounted = false;
        }

        set cmds(values) {
          this.commands = values;
          this.render();
        }

        static get observedAttributes() {
          return ["width", "height"];
        }

        connectedCallback() {
          // Wait for the inner elements to be rendered before using them
          requestAnimationFrame(() => {
            this.canvas = this.querySelector("canvas");
            this.context = this.canvas.getContext("2d");
            this.mounted = true;

            this.setCanvasDimensions();
            this.render();
          });
        }

        attributeChangedCallback(name, oldValue, newValue) {
          if (
            (name === "width" || name === "height") &&
            oldValue !== newValue
          ) {
            // Wait for Elm to finish rendering and setting its stuff before
            // changing the inner canvas dimensions
            requestAnimationFrame(() => {
              this.setCanvasDimensions();
              this.render();
            });
          }
        }

        setCanvasDimensions() {
          if (!this.mounted) return;

          // Get dimensions from the elm-canvas element. If they are not set, try to
          // get them from the canvas element inside (to support elm-canvas@3.0.3)
          var width = Number(
            this.getAttribute("width") || this.canvas.getAttribute("width")
          );
          var height = Number(
            this.getAttribute("height") || this.canvas.getAttribute("height")
          );

          var devicePixelRatio = window.devicePixelRatio || 1;
          this.canvas.style.width = width + "px";
          this.canvas.style.height = height + "px";
          this.canvas.width = width * devicePixelRatio;
          this.canvas.height = height * devicePixelRatio;
          // Reset current transformation matrix to the identity matrix
          this.context.setTransform(1, 0, 0, 1, 0, 0);
          this.context.scale(devicePixelRatio, devicePixelRatio);
        }

        render() {
          if (!this.mounted) return;
          // Iterate over the commands in reverse order as that's how the Elm side
          // builds them with linked lists
          for (let i = this.commands.length - 1; i >= 0; i--) {
            this.execCommand(this.commands[i]);
          }
        }

        execCommand(cmd) {
          if (cmd.type === "function") {
            this.context[cmd.name](...cmd.args);
          } else if (cmd.type === "field") {
            this.context[cmd.name] = cmd.value;
          } else if (cmd.type === "variable") {
            (function(context) {
              if (cmd.init.type === "function") {
                const localVar = context[cmd.init.name](...cmd.init.args);
                const modifiers = cmd.modifiers;
                for (let i = 0; i < modifiers.length; i++) {
                    if (modifiers[i].type === "function") {
                      localVar[modifiers[i].name](...modifiers[i].args);
                    }
                }
                context[cmd.field] = localVar;
              }
            })(this.context); // Closure
          }
        }
      }
    );
  } else {
    throw new Error(
      "window.customElements does not exist. Please use an appropriate polyfill"
    );
  }
}
