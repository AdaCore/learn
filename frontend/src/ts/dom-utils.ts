/**
 * Safely gets an element by its id from the document.
 *
 * If an element with the ID is not found, an exception is thrown.
 *
 * @export
 * @param {string} id - The id to find
 * @return {HTMLElement} - The found element
 */
export function getElemById(id: string): HTMLElement {
  const res = document.getElementById(id);
  if (res == null) {
    throw Error('Malformed DOM. Cannot find elem ' + id);
  }
  return res;
}
/**
 * Safely gets an attribute from an element.
 *
 * If the attribute is not found, an exception is thrown.
 *
 * @export
 * @param {HTMLElement} elem - The element to query
 * @param {string} attr - The attribute name to get
 * @return {string} - The attribute value
 */
export function getElemAttr(elem: HTMLElement, attr: string): string {
  const res = elem.getAttribute(attr);
  if (res == null) {
    throw Error('Malformed elem. Cannot get attribute: ' + attr);
  }

  return res;
}
/**
 * Gets all child elements of root with a speciic class.
 *
 * This wraps the HTMLCollectionOf, which is the return of
 * getElementsByClassName, in an array so that it is iterable.
 *
 * @export
 * @param {(HTMLElement | Document)} root - The root element to search
 * @param {string} name - The class name to find
 * @return {Array<HTMLElement>} - The list of found elements with the class
 */
export function getElemsByClass(root: HTMLElement | Document, name: string):
    Array<HTMLElement> {
  const coll = root.getElementsByClassName(name) as
      HTMLCollectionOf<HTMLElement>;
  return Array.from(coll);
}
/**
 * Gets all elements of root with a specific tag.
 *
 * This wraps the HTMLCollectionOf, which is the return of
 * getElementByTagName, in an array so that it is iterable.
 *
 * @export
 * @param {(HTMLElement | Document)} root - The root element to search
 * @param {string} tag - The tag name to find
 * @return {Array<HTMLElement>} - The list of found elements with the tag
 */
export function getElemsByTag(root: HTMLElement | Document, tag: string):
    Array<HTMLElement> {
  const coll = root.getElementsByTagName(tag) as HTMLCollectionOf<HTMLElement>;
  return Array.from(coll);
}
