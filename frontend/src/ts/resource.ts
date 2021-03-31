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
