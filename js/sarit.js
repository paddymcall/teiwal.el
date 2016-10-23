/***********************************
 * Behaviors, i.e. ways you want specific elements to be handled that
 * you can't do with CSS, can be injected by calling the addBehaviors
 * method, passing it an object containing a set of handler functions
 * and fallback functions (for browsers that don't support Web
 * Components). Handler functions must take a single parameter which
 * is an HTML element prototype, and fallback functions take a single
 * parameter which is a copy of the DOM containing the elements to be
 * handled. The function names must match the name of the element they
 * will be applied to.
 ***********************************/
// a running number for notes
var noteCount = 0;
var saritBehaviors =     {
    "handlers" : {
	// Overrides the default ptr behavior, displaying a short link
	"ptr": function() {
            return function() {
		var shadow = this.createShadowRoot();
		var link = document.createElement("a");
		link.innerHTML = this.getAttribute("target").replace(/https?:\/\/([^\/]+)\/.*/, "$1");
		link.href = this.getAttribute("target");
		shadow.appendChild(link);
            };
	},
	// Adds a new handler for <term>, wrapping it in an HTML <b>
	"term": function() {
            return function() {
		var shadow = this.createShadowRoot();
		var b = document.createElement("b");
		b.innerHTML = this.innerHTML;
		shadow.appendChild(b);
            };
	},
	// Inserts the first array element before tei:add, and the second, after.
	"add": ["`","´"],
	// Work with notes
	"note": function() {
            return function() {
		var shadow = this.createShadowRoot();
		// count the note
		noteCount = noteCount + 1;
		// make sure we have a global id (also in the light
		// dom, not just in the shadow context here)
		var noteId;
		if (this.hasAttribute("xml:id")) {
		    noteId = this.getAttribute("xml:id");
		} else {
		    // guess an id:
		    noteId = "auto-note-" + noteCount.toString();
		    this.id = noteId;
		}
		// set up a link element to the content of the note,
		// and a section for the content 
		var link = document.createElement("a");
		var note = document.createElement("section");
		note.className = "note";
		link.className = "note";
		link.href = "#" + noteId;
		// use running number for display
		link.innerHTML = "[" + noteCount + "]";
		note.innerHTML = this.innerHTML + `<a href="#" class="closebtn">×</a>`;
		// glue things together:
		// add a style to the shadow root (applies in the shadow)
		shadow.innerHTML = `
<style>

a.note { background: orange; 
	 display: inline;
       }

section.note {
    display: none;
}

/* close & open on target selection; see http://www.w3schools.com/cssref/sel_target.asp */
/* The button used to close the modal */
.closebtn {
  text-decoration: none;
  float: right;
  font-size: 35px;
  font-weight: bold;
}

/* host = tei-note element */
:host(:target) section.note { 
  display: block;
  margin: 1.5em auto 1.5em auto;
  padding: .5em 1.5em 1.5em 1.5em;
  border: 1px solid black;
  border-radius: 15px;
  width: 325px;
  position: relative;
  border: 1px solid #aaaaaa;
  background: #fafafa;
}

</style>
`;
		shadow.appendChild(link);
		shadow.appendChild(note);
            };
	}
    },
    "fallbacks" : {
	"ptr": function(elt) {
	    var content = document.createElement("a");
	    content.setAttribute("href", elt.getAttribute("target"));
	    content.innerHTML = elt.getAttribute("target").replace(/https?:\/\/([^\/]+)\/.*/, "$1");
	    elt.appendChild(content);
	    elt.addEventListener("click", function(event) {
		window.location = this.getAttribute("target");
	    });
	},
	// Note that this would be better done with CSS. Included for completeness.
	"term": function(elt) {
	    elt.setAttribute("style", "font-weight: bold");
	}

    }
};





