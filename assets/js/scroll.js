document.addEventListener('DOMContentLoaded', function() {
    // Intersection Observer Options
    var options = {
        root: null,
        rootMargin: "0px",
        threshold: [0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0],
    };

    // Each Intersection Observer runs setCurrent
    var observeHtags = new IntersectionObserver(setCurrent, options);

    // Build the DOM for the menu
    function createTOC() {
        /*
        var frag = document.createDocumentFragment();
        var jsNav = document.createElement("nav");
        jsNav.classList.add("toc-Wrapper");
        var tocTitle = document.createElement("h4");
        tocTitle.classList.add("toc-Title");
        tocTitle.textContent = "Sections";
        jsNav.appendChild(tocTitle);
        allHtags.forEach((el, i) =>  {
            var links = document.createElement("a");
            links.setAttribute("href", "#h-" + el.tagName + "_" + i);
            links.classList.add("toc-Link");
            links.classList.add("toc-Link_" + el.tagName);
            var textContentOfLink = el.textContent;
            el.id = "h-" + el.tagName + "_" + i;
            links.textContent = textContentOfLink;
            frag.appendChild(links);
        });
        jsNav.appendChild(frag);
        insertNode.appendChild(jsNav);
        */
        // Now
        allHtags.forEach(tag => {
            observeHtags.observe(tag);
        });
    }

    (function setUp() {
        var allHtags = document.querySelectorAll("section");
        //console.log(allHtags);
        allHtags.forEach(tag=>{
            observeHtags.observe(tag);
        });
    })();

    function didThisIntersectionHappenAtTop(i) {
        return i.rootBounds.bottom - i.boundingClientRect.bottom > i.rootBounds.bottom / 2 ? true
     : false
    }

    function getHeadingIndex(i) {
        let priorEle = (ele) => ele === i.target;
        return allHtags.findIndex(priorEle);
    }

    function getPriorHeading(i) {
        let priorEle = (ele) => ele === i.target;
        return allHtags.findIndex(priorEle);
    }

    var values = {};
    var keep_focus = null;

    // Function that runs when the Intersection Observer fires
    function setCurrent(e) {
        //console.log(e);
        e.map(i => {
            let r = i.intersectionRatio;
            if (i.target.className === "level1") r /= 15;
            values[i.target.id] = r;
        });
        requestAnimationFrame(() => {
            let TOC = document.querySelector("#TOC");
            let id = Object.keys(values).reduce((a, b) => values[a] >= values[b] ? a : b);
            //console.log(id, values);
            let orig = document.querySelector(`#TOC a[href="#${id}"]`);
            if (orig === keep_focus) return;
            if (!orig) {
                var allSectionLinks = document.querySelectorAll("#TOC a[href]");
                allSectionLinks.forEach(link => {
                        link.parentElement.classList.remove("current");
                });
                keep_focus = orig;
                return;
            }
            let anchor = orig;
            while (anchor && anchor.offsetParent === null) {
                while (anchor && !anchor.nextElementSibling) anchor = anchor.parentElement;
                if (anchor) anchor = anchor.nextElementSibling;
            }
            if (anchor) {
                var save = anchor.offsetTop - TOC.scrollTop;
                //console.log(id, anchor, anchor.offsetTop);
            }
            var allSectionLinks = document.querySelectorAll("#TOC a[href]");
            allSectionLinks.forEach(link => {
                    link.parentElement.classList.remove("current");
            });
            let target = orig.parentElement;
            target.classList.add("current");
            while ((target = target?.parentElement?.parentElement)?.nodeName === "LI") {
                target.classList.add("current");
            }
            if (anchor) {
                let goTo = anchor.offsetTop - save;
                if (TOC.scrollTop !== goTo) TOC.scrollTop = goTo;
                //console.log(anchor.offsetTop, save);
            }
            //setTimeout(100, () => orig.scrollIntoView());
            //orig.scrollIntoView({block: "nearest", inline: "nearest", scrollMode: "if-needed"});
            if (orig !== keep_focus) {
                let frac = orig.offsetTop / TOC.scrollHeight;
                //console.log(Array.prototype.indexOf.call(allSectionLinks, orig));
                let ix = Array.prototype.indexOf.call(allSectionLinks, orig);
                frac = ix / allSectionLinks.length;
                let goTo = orig.offsetTop + frac*orig.offsetHeight - frac*TOC.offsetHeight;
                if (ix === 0) goTo = 0;
                TOC.scrollTo({ top: goTo, behavior: e[0].time < 1000 ? "auto" : "smooth" });
                keep_focus = orig;
            }
        });
    }
});
