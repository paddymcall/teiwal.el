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
		var link = document.createElement("a");
		var note = document.createElement("section");
		note.className = "note";
		link.className = "note";
		note.id = this.getAttribute("xml:id") || "note-" + (function () {
		    noteCount = noteCount + 1;
		    return noteCount;
		})();
		link.href = "#" + note.id;
		link.innerHTML = "[" + note.id.toString().replace(/^note-/, "") + "]";
		note.innerHTML = this.innerHTML;
		// glue things together
		shadow.innerHTML = `
<style>
a.note { background: orange; 
	 display: inline;
       }

section.note {
    display: none;
}

a.note:active ~ section.note {
    display: block; background: yellow;
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





