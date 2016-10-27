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
// a running number for line groups
var lgCount = 0;
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
	"lg": ["", "<div class=\"verse-num\">[$@n]</div>"],
	// Inserts the first array element before tei:add, and the second, after.
	"add": ["`","´"],
	"pb": function() {
	    return function() {
		console.log("PB matched: ", this);
		var shadow = this.createShadowRoot();
		var span = document.createElement("span");
		var ed = "[main]";
		var num = "pb";
		span.className = "pb";
		if (this.hasAttribute("edRef")) {
		    ed = this.getAttribute("edRef");
		} else if (this.hasAttribute("ed")) {
		    ed = this.getAttribute("ed");
		}
		if (this.hasAttribute("n")) {
		    num = "p. " + this.getAttribute("n");
		} 
		span.innerHTML = ed + "/" + num;
		shadow.appendChild(span);
		shadow.appendChild(this);
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
	},
	"pb": function(elt) {
	    // console.log("PB matched: ", elt);
	    var span = document.createElement("span");
	    var ed = "main";
	    var num = "pb";
	    span.className = "pb";
	    if (elt.hasAttribute("edRef")) {
		ed = elt.getAttribute("edRef");
	    } else if (elt.hasAttribute("ed")) {
		ed = elt.getAttribute("ed");
	    }
	    if (elt.hasAttribute("n")) {
		num = elt.getAttribute("n");
	    } 
	    span.innerHTML = ed + "/" + num;
	    elt.appendChild(span);
	},
	"lb": function (elt) {
	    var span = document.createElement("span");
	    var ed = "main";
	    var num = "lb";
	    span.className = "lb";
	    if (elt.hasAttribute("edRef")) {
		ed = elt.getAttribute("edRef");
	    } else if (elt.hasAttribute("ed")) {
		ed = elt.getAttribute("ed");
	    }
	    if (elt.hasAttribute("n")) {
		num = elt.getAttribute("n");
	    } 
	    span.innerHTML = ed + "/" + num;
	    // to insert before: elt.parentElement.insertBefore(span, elt);
	    elt.appendChild(span);
	},
	"lg": function (elt) {
	    var num = (function () { lgCount = lgCount + 1; return lgCount;})();
	    if (elt.hasAttribute("n")) {
		num = elt.getAttribute("n");
	    } else {
		num = lgCount + " (running number)";
	    }
	    var numDiv = document.createElement("div");
	    numDiv.className = "verse-num";
	    numDiv.innerHTML = "[v. " + num + "]";
	    elt.appendChild(numDiv);
	}
    }
};



// global callback function
var saritCallback = function (x) {
    console.log("saritCallback called!");
};

// per element callback function
var saritPerElementFunc = function (x) {
    // switch (x.localName) {
    // case "tei-note":
    // 	// add an e element, but inside the note
    // 	(function (note) {
    // 	    // parent is null at this point:
    // 	    // console.log("Parent: " + note.parentElement);
    // 	    noteCount = noteCount + 1;
    // 	    var noteRef = document.createElement("a");
    // 	    noteRef.href = "#note-" + noteCount;
    // 	    noteRef.className = "note";
    // 	    noteRef.innerHTML = "[n]";
    // 	    note.appendChild(noteRef);
    // 	})(x);
    // 	break;
    // };
};


// the setup function
var saritSetup = function (teidoc) {
    var c = new CETEI();
    c.addBehaviors(saritBehaviors);
    c.getHTML5(teidoc,// the document to use
	       // the callback function
	       function(data) {
		   var root = document.getElementById("TEI");
		   while (root.firstChild) {
		       root.removeChild(root.firstChild);
		   };
		   root.appendChild(data);
		   saritInsertNavList();
	       }
	       // ,
	       // something to do with each element during
	       // construction
	       //saritPerElementFunc
	      );
};

var saritInsertNavList = function () {
    var headers = document.getElementsByTagName("tei-head");
    var items = [];
    // console.log("Headers found: ", headers.length);
    if (headers.length > 0) {
	var factor = $(headers[0]).parentsUntil("#TEI").length - 1;
	for (var i = 0, header; header = headers[i]; i++) {
	    // console.log("Looping: index ", i, "header ", header);
	    // find parent element (often, a div) and an id to link to
	    var parent = header.parentElement;
	    var id;
	    if (header.id) {
		id = header.id;
	    } else if (parent.id) {
		id = parent.id;
	    } else {
		id = "temp-header-" + i;
		header.id = id;
	    }
	    var link = {};
	    link['to'] = "#" + id;
	    link['depth'] = $(header).parentsUntil("#TEI").length - factor;
	    link['text'] = header.innerHTML;
	    items.push(link);

	    // link.addClass($(header).parentsUntil("#TEI").length.toString());
	}
	// build a list
	var ol = "";
	var stack = [];
	for (var j = 0, li; li = items[j]; j++) {
	    // stack.push([li["to"], li["depth"], li["text"], j ]);
	    // figure which ol to append to
	    if (j == 0) {
	    	// console.log("First item");
	    	ol = "<ol><li><a href=\"" + li["to"] + "\">" + li["text"] + "</a>\n";
	    } else if (li['depth'] == items[j-1]['depth']) {
	    	// same list as before; just append a new li + a;
	    	// console.log("Same depth as before: ", li, "list: ", ol);
	    	ol = ol + "</li><li><a href=\"" + li["to"] + "\">" + li["text"] + "</a>\n";
	    } else if (li['depth'] > items[j-1]['depth']) {
	    	// one (or more) levels deeper: add checkbox, open new ol, add li + a
	    	ol = ol + "<input type=\"checkbox\" id=\"nav" + j + "\" />" +
	    	    "<ol><li><a href=\"" + li["to"] + "\">" + li["text"] + "</a>\n";
	    	// console.log("Deeper than before: ", li, "list: ", ol);
	    } else {
	    	// up one or more levels: close last li, last ol, outer li, and append new li + a
	    	// console.log("Level up here: ", li, "list: ", ol);
		// console.log("Closing up: ", li["depth"], " vs. ", items[j-1]["depth"]);
		for (var k=0; k < (items[j-1]["depth"] - li["depth"]); k++) {
		    ol = ol + "</li></ol>";
		}
	    	ol = ol + "</li><li><a href=\"" + li["to"] + "\">" + li["text"] + "</a>\n";
	    }
	}
	// console.log("Stack is: ", JSON.stringify(stack, null, " "));
	
	// console.log("Nav list is: ", ol, "</li></ol>" );
	document.getElementById("nav").innerHTML = ol + "</li></ol>";
    }

};
