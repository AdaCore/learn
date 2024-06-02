/**
 * Safely gets an element by its id from the document.
 * If an element with the ID is not found, an exception is thrown.
 * @param {string} id - The id to find
 * @returns {HTMLElement} - The found element
 */
export function getElemById(id: string): HTMLElement {
  const res = document.getElementById(id);
  if (res == null) {
    throw Error('Malformed DOM. Cannot find elem ' + id);
  }
  return res;
}

/**
 * Gets all child elements of root with a specific class.
 * This wraps the HTMLCollectionOf, which is the return of
 * getElementsByClassName, in an array so that it is iterable.
 * @param {(HTMLElement | Document)} root - The root element to search
 * @param {string} name - The class name to find
 * @returns {Array<HTMLElement>} - The list of found elements with the class
 */
export function getElemsByClass(root: HTMLElement | Document, name: string):
    Array<HTMLElement> {
  const coll = root.getElementsByClassName(name) as
      HTMLCollectionOf<HTMLElement>;
  return Array.from(coll);
}
/**
 * Gets all elements of root with a specific tag.
 * This wraps the HTMLCollectionOf, which is the return of
 * getElementByTagName, in an array so that it is iterable.
 * @param {(HTMLElement | Document)} root - The root element to search
 * @param {string} tag - The tag name to find
 * @returns {Array<HTMLElement>} - The list of found elements with the tag
 */
export function getElemsByTag(root: HTMLElement | Document, tag: string):
    Array<HTMLElement> {
  const coll = root.getElementsByTagName(tag) as HTMLCollectionOf<HTMLElement>;
  return Array.from(coll);
}
