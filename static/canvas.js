canvas = document.getElementById('theImage');
context = canvas.getContext('2d');
img = new Image();
img.onload = function () {

    /// set size proportional to screen
    canvas.height = screen.height * 0.75;
    canvas.width = screen.width * 0.9;

    img.width = canvas.width;
    img.height = canvas.height;
    context.drawImage(img, 0, 0, img.width, img.height);

    /// step 2 - resize 50% of step 1
    //octx.drawImage(oc, 0, 0, oc.width * 0.5, oc.height * 0.5);

    /// step 3, resize to final size
    //octx.drawImage(oc, 0, 0, oc.width * 0.5, oc.height * 0.5,
    //0, 0, canvas.width, canvas.height);
}
img.src = '../image/Draw?d=' + Date.now();
showPic = function() {
    //img.src = '../image/Draw?d=' + Date.now();
    //resizeCanvas();
    console.log("redrawing imag");
    context.drawImage(img, 0, 0);
}
handlePic = function(f) {
    var oReq = new XMLHttpRequest();
    oReq.onreadystatechange = function() {
        if (oReq.readyState == 4 && oReq.status == 200) showPic();
    };
    oReq.open('POST', '../postImg/True', true);
    oReq.setRequestHeader('Content-Type', 'image/jpeg');
    console.log("sending imag");
    oReq.send(f);
};
handlePic2 = function(f,b) {
    var oReq = new XMLHttpRequest();
    oReq.onreadystatechange = function() {
        if (oReq.readyState == 4 && oReq.status == 200) showPic2();
    };
    oReq.open('POST', '../postImg/' + b, true);
    oReq.setRequestHeader('Content-Type', 'image/jpeg');
    console.log("sending imag");
    oReq.send(f);
};
picUpdate = function() {
    canvas.toBlob(function(f) {
        handlePic(f);
	img.src = '../image/Draw?d=' + Date.now();
    }, 'image/jpeg', 1);
    console.log("saving imag");
};

//functions for drawing

drawing = function() {
    var cnvs = document.getElementById('theImage');
    var ctx = cnvs.getContext('2d');
    ctx.fillCircle = function(x, y, radius, fillColor) {
        this.fillStyle = fillColor;
        this.beginPath();
        this.moveTo(x, y);
        this.arc(x, y, radius, 0, Math.PI * 2, false);
        this.fill();
    };
    ctx.clearTo = function(fillColor) {
        ctx.fillStyle = fillColor;
        ctx.fillRect(0, 0, img.width, img.height);
    };
    ctx.clearTo('#ddd');
    var isDrawing = false;
    cnvs.onmousemove = function(e) {
        if (!isDrawing) {
	    return;
        }
        var x = e.pageX - this.offsetLeft;
        var y = e.pageY - this.offsetTop;
        var radius = 2;
        var fillColor = '#f00';
        ctx.fillCircle(x, y, radius, fillColor);
    };
    cnvs.onmousedown = function(e) {
        isDrawing = true;
    };
    cnvs.onmouseup = function(e) {
        isDrawing = false;
    };
};
var ongoingTouches = [];
function handleStart(evt) {
    evt.preventDefault();
    console.log("touchstart.");
    var el = document.getElementsByTagName("canvas")[0];
    var ctx = el.getContext("2d");
    var touches = evt.changedTouches;
    
    for (var i = 0; i < touches.length; i++) {
	console.log("touchstart:" + i + "...");
	ongoingTouches.push(copyTouch(touches[i]));
	var color = colorForTouch(touches[i]);
	ctx.beginPath();
	//ctx.arc(touches[i].pageX, touches[i].pageY, 4, 0, 2 * Math.PI, false);  // a circle at the start
	ctx.fillStyle = color;
	ctx.fill();
	console.log("touchstart:" + i + ".");
    }
}
function handleEnd(evt) {
    evt.preventDefault();
    console.log("touchend");
    var el = document.getElementById("theImage");
    var ctx = el.getContext("2d");
    var touches = evt.changedTouches;

    for (var i = 0; i < touches.length; i++) {
	var color = colorForTouch(touches[i]);
	var idx = ongoingTouchIndexById(touches[i].identifier);

	if (idx >= 0) {
	    ctx.lineWidth = 2;
	    ctx.fillStyle = color;
	    ctx.beginPath();
	    ctx.moveTo(ongoingTouches[idx].pageX, ongoingTouches[idx].pageY);
	    ctx.lineTo(touches[i].pageX, touches[i].pageY);
	    // ctx.fillRect(touches[i].pageX - 4, touches[i].pageY - 4, 8, 8);  // and a square at the end
	    ongoingTouches.splice(idx, 1);  // remove it; we're done
	} else {
	    console.log("can't figure out which touch to end");
	}
    }
}
function handleMove(evt) {
    evt.preventDefault();
    var el = document.getElementById("theImage");
    var ctx = el.getContext("2d");
    var touches = evt.changedTouches;

    for (var i = 0; i < touches.length; i++) {
	var color = colorForTouch(touches[i]);
	var idx = ongoingTouchIndexById(touches[i].identifier);

	if (idx >= 0) {
	    log("continuing touch "+idx);
	    ctx.beginPath();
	    log("ctx.moveTo(" + ongoingTouches[idx].pageX + ", " + ongoingTouches[idx].pageY + ");");
	    ctx.moveTo(ongoingTouches[idx].pageX, ongoingTouches[idx].pageY);
	    log("ctx.lineTo(" + touches[i].pageX + ", " + touches[i].pageY + ");");
	    ctx.lineTo(touches[i].pageX, touches[i].pageY);
	    ctx.lineWidth = 4;
	    ctx.strokeStyle = color;
	    ctx.stroke();

	    ongoingTouches.splice(idx, 1, copyTouch(touches[i]));  // swap in the new touch record
	    console.log(".");
	} else {
	    console.log("can't figure out which touch to continue");
	}
    }
}

function onTouch(evt) {
    evt.preventDefault();
    if (evt.touches.length > 1 || (evt.type == "touchend" && evt.touches.length > 0))
	return;

    var newEvt = document.createEvent("MouseEvents");
    var type = null;
    var touch = null;

    switch (evt.type) {
    case "touchstart": 
	type = "mousedown";
	touch = evt.changedTouches[0];
	break;
    case "touchmove":
	type = "mousemove";
	touch = evt.changedTouches[0];
	break;
    case "touchend":        
	type = "mouseup";
	touch = evt.changedTouches[0];
	break;
    }

    newEvt.initMouseEvent(type, true, true, evt.originalTarget.ownerDocument.defaultView, 0, touch.screenX, touch.screenY, touch.clientX, touch.clientY, evt.ctrlKey, evt.altKey, evt.shiftKey, evt.metaKey, 0, null);
    evt.originalTarget.dispatchEvent(newEvt);
}
function handleMove(evt) {
    evt.preventDefault();
    var el = document.getElementById("theImage");
    var ctx = el.getContext("2d");
    var touches = evt.changedTouches;

    for (var i = 0; i < touches.length; i++) {
	var color = colorForTouch(touches[i]);
	var idx = ongoingTouchIndexById(touches[i].identifier);

	if (idx >= 0) {
	    console.log("continuing touch "+idx);
	    ctx.beginPath();
	    ctx.moveTo(ongoingTouches[idx].pageX, ongoingTouches[idx].pageY);
	    ctx.lineTo(touches[i].pageX, touches[i].pageY);
	    ctx.lineWidth = 4;
	    ctx.strokeStyle = color;
	    ctx.stroke();

	    ongoingTouches.splice(idx, 1, copyTouch(touches[i]));  // swap in the new touch record
	    console.log(".");
	} else {
	    console.log("can't figure out which touch to continue");
	}
    }
}
function handleCancel(evt) {
    evt.preventDefault();
    console.log("touchcancel.");
    var touches = evt.changedTouches;
    
    for (var i = 0; i < touches.length; i++) {
	var idx = ongoingTouchIndexById(touches[i].identifier);
	ongoingTouches.splice(idx, 1);  // remove it; we're done
    }
}
function colorForTouch(touch) {
    var r = touch.identifier % 16;
    var g = Math.floor(touch.identifier / 3) % 16;
    var b = Math.floor(touch.identifier / 7) % 16;
    r = r.toString(16); // make it a hex digit
    g = g.toString(16); // make it a hex digit
    b = b.toString(16); // make it a hex digit
    var color = "#" + r + g + b;
    console.log("color for touch with identifier " + touch.identifier + " = " + color);
    return color;
}
function copyTouch(touch) {
    return { identifier: touch.identifier, pageX: touch.pageX, pageY: touch.pageY };
}

function ongoingTouchIndexById(idToFind) {
    for (var i = 0; i < ongoingTouches.length; i++) {
	var id = ongoingTouches[i].identifier;
	if (id == idToFind) {
	    return i;
	}
    }
    return -1;    // not found
}

var el = document.getElementById("theImage");
el.addEventListener("touchstart", handleStart, false);
el.addEventListener("touchend", handleEnd, false);
el.addEventListener("touchcancel", handleCancel, false);
el.addEventListener("touchmove", handleMove, false);
drawing();
