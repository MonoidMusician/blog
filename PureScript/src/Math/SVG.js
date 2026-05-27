export const _parseMIME = mimetype => source => {
  try {
    const parsed = new DOMParser().parseFromString(source, mimetype);
    return [XMLNode(parsed)];
  } catch (e) {
    console.error(e);
    return [];
  }
};
const XMLNode = (/** @type SVGGraphicsElement */ node) => {
  const array = v => v == null ? [] : [v];
  // https://github.com/w3c/svgwg/issues/706
  const fromMat = mkMat => mat => (mkMat
    (mat.a  )(mat.b  )(mat.m13)(mat.m14)
    (mat.c  )(mat.d  )(mat.m23)(mat.m24)
    (mat.m31)(mat.m32)(mat.m33)(mat.m34)
    (mat.e  )(mat.f  )(mat.m43)(mat.m44)
  );
  return {
    tagName: node.tagName,
    namespaceURI: node.namespaceURI,
    parent: () => node.parentElement ? [XMLNode(node.parentElement)] : [],
    childNodes: wrap => {
      return [...node.childNodes].map(n => {
        switch (n.nodeType) {
          case Node.TEXT_NODE:
          case Node.CDATA_SECTION_NODE:
            return wrap.text(n.nodeValue);
          case Node.COMMENT_NODE:
          case Node.PROCESSING_INSTRUCTION_NODE:
          case Node.DOCUMENT_TYPE_NODE:
            return wrap.comment(n.nodeValue);
          default:
            return wrap.node(XMLNode(n));
        }
      });
    },
    querySelector: query => array(node.querySelector(query)).map(XMLNode),
    querySelectorAll: query => [...node.querySelectorAll(query)].map(XMLNode),
    xpath: query => {
      const snapshot = new XPathEvaluator().evaluate(
        query,
        node,
        node,
        XPathResult.ORDERED_NODE_SNAPSHOT_TYPE,
      );
      const length = snapshot.snapshotLength;
      const results = new Array(length);
      for (let i=0; i<length; i++)
        results[i] = XMLNode(snapshot.snapshotItem(i));
      return results;
    },
    getAttributes: () => {
      const attrs = node.attributes;
      const length = attrs.length;
      const results = new Array(length);
      for (let i=0; i<length; i++) {
        const attr = attrs.item(i);
        results[i] = {
          name: attr.name,
          localName: attr.localName,
          namespaceURI: attr.namespaceURI,
          value: attr.value,
        };
      }
      return results;
    },
    getAttribute: nses => name => array(node.getAttributeNS(nses[0], name)),
    getPathData: normalize => node.getPathData({ normalize }),
    serializeToXML: () => new XMLSerializer().serializeToString(node),

    getTransformListAs: mkMat => [...node.transform.baseVal].map(fromMat(mkMat)),
    getBBox: options => ({...node.getBBox(options)}),
    getCTMAs: mkMat => fromMat(mkMat)(node.getCTM()),
  };
};
