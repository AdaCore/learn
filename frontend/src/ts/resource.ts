/**
 * Corresponds to a text file
 *
 * @export
 */
export type Resource = {
  basename: string;
  contents: string;
}

/**
 * Corresponds to a list of Resources
 *
 * @export
 */
export type ResourceList = Array<Resource>;

/**
 * A Set of resources
 *  A set is a list of unique items. Here, uniqueness corresponds to the
 *  resource's basename only. Contents are ignored for equality.
 *
 * @export
 * @class ResourceSet
 * @implements {Iterable<Resource>}
 */
export class ResourceSet implements Iterable<Resource> {
  private data: ResourceList = [];
  private counter = 0;

  /**
   * Allows the ResourceSet to be Iterable
   *
   * @return {IteratorResult<Resource>}
   */
  public next(): IteratorResult<Resource> {
    if (this.counter < this.data.length) {
      return {
        done: false,
        value: this.data[this.counter++],
      };
    } else {
      this.counter = 0;
      return {
        done: true,
        value: null,
      };
    }
  }

  /**
   * Allows the ResourceSet to be looped over
   *
   * @return {IterableIterator<Resource>}
   */
  [Symbol.iterator](): IterableIterator<Resource> {
    return this;
  }

  /**
   * Gives the length of the set
   *
   * @readonly
   * @return {number} - The length of the set
   */
  get length(): number {
    return this.data.length;
  }

  /**
   * Gives the underlying array
   *
   * @readonly
   * @return {ResourceList} - The underlying array
   */
  get raw(): ResourceList {
    return this.data;
  }

  /**
   * Find a matching Resource in the set. Returns null if not found.
   *
   * @private
   * @param {Resource} file - THe file (basename) to find
   * @return {(Resource | null)} - Returns the resource if found
   */
  private find(file: Resource): Resource | null {
    for (const f of this.data) {
      if (f.basename === file.basename) {
        return f;
      }
    }

    return null;
  }

  /**
   * Weakly adds a Resource to the set.
   *  If an existing member of the set has the same name as the file to add
   *  the new file is ignored and the subprogram returns false. If the new file
   *  is unique in its name, the file is added and the subprogram returns true.
   *
   * @param {Resource} file - The Resource to add to the set
   * @return {boolean} - Returns true if the Resource was added.
   */
  public addWeak(file: Resource): boolean {
    if (this.find(file)) {
      return false;
    }

    this.data.push(file);
    return true;
  }

  /**
   * Uniquely adds a Resource to the set.
   *  If a member in the set has the same name as the file to add, an exception
   *  is thrown. Otherwise, the file is added.
   *
   * @throws {Error} - When the file already exists in the set
   * @param {Resource} file - The file to add to the set
   */
  public addUnique(file: Resource): void {
    if (!this.addWeak(file)) {
      throw Error('key: ' + file.basename + ' already in Set.');
    }
  }

  /**
   * Adds a file to the set.
   *  If a meber in the set has the same name as the file to add, the new
   *  contents overwrite the existing contents and the subprogram returns false
   *
   * @param {Resource} file - The file to add
   * @return {boolean} - False if the file is overwriting an existing member.
   */
  public addOverwrite(file: Resource): boolean {
    const found = this.find(file);
    if (found) {
      found.contents = file.contents;
      return false;
    }

    this.data.push(file);
    return true;
  }

  /**
   * Merges a list in this list. Priority is given to the passed in list.
   *
   * @param {ResourceSet} files - The files to overwrite
   */
  public addListOverwrite(files: ResourceSet): void {
    for(const f of files) {
      this.addOverwrite(f);
    }
  }
}
