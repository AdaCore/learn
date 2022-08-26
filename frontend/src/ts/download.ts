import JSZip from 'jszip';
import FileSaver from 'file-saver';
import {ResourceList} from './resource';

const COMMON_ADC = `pragma Restrictions (No_Specification_of_Aspect => Import);
pragma Restrictions (No_Use_Of_Pragma => Import);
pragma Restrictions (No_Use_Of_Pragma => Interface);
pragma Restrictions (No_Use_Of_Pragma => Linker_Options);
pragma Restrictions (No_Dependence => System.Machine_Code);
pragma Restrictions (No_Dependence => Machine_Code);\n`;

const MAIN_GPR = `project Main is

   --MAIN_PLACEHOLDER--

   --LANGUAGE_PLACEHOLDER--

   package Compiler is
      for Default_Switches ("Ada") use ("-g", "-O0", "-gnata", "-gnatwa");
      --COMPILER_SWITCHES_PLACEHOLDER--
   end Compiler;

   package Builder is
      for Default_Switches ("Ada") use ("-g");
      --BUILDER_SWITCHES_PLACEHOLDER--
      for Global_Configuration_Pragmas use "main.adc";
   end Builder;

end Main;\n`;

const languageMapping = {
  '"Ada"': ['.adb', 'ads'],
  '"c"': ['.c', '.h'],
  '"c++"': ['.cpp', '.hh'],
};

const cMainRegExp = /^(?:void|int) +main\(.*\)(?: |\n)*{/gm;

interface UnparsedSwitches {
  Builder: Array<string>;
  Compiler: Array<string>;
}

interface ParsedSwitches {
  '--BUILDER_SWITCHES_PLACEHOLDER--': string;
  '--COMPILER_SWITCHES_PLACEHOLDER--': string;
}

/**
 * Generate the value to replace the language placeholder in the gpr file.
 * @param {ResourceList} files list of files to check.
 * @return {string} the value to replace the language placeholder.
 */
export function getLanguages(files: ResourceList): string {
  const languages = new Set<string>();
  for (const f of files) {
    for (const [language, extensions] of Object.entries(languageMapping)) {
      for (const ext of extensions) {
        if (f.basename.endsWith(ext)) languages.add(language);
      }
    }
  }
  return `for Languages use (${Array.from(languages).join(', ')});`;
}

/**
 * Generate the values to replace the switch placeholders in the gpr file.
 * @param {string} rawSwitches provided by data-switches for the project.
 * @return {Switches} parsed switches.
 */
export function parseSwitches(rawSwitches: string): ParsedSwitches {
  const parsed = JSON.parse(rawSwitches);
  const switches: UnparsedSwitches = {Builder: [], Compiler: []};
  for (const k in switches) {
    if (k in parsed) {
      const updatedSwitches = [];
      for (const origSwitch of parsed[k]) {
        updatedSwitches.push(`"${origSwitch}"`);
      }
      switches[k as keyof UnparsedSwitches] = updatedSwitches;
    }
  }
  const builder =
    `for Switches ("Ada") use (${switches['Builder'].join(', ')});`;
  const compiler =
    `for Switches ("Ada") use (${switches['Compiler'].join(', ')});`;
  return {
    '--BUILDER_SWITCHES_PLACEHOLDER--': builder,
    '--COMPILER_SWITCHES_PLACEHOLDER--': compiler,
  };
}

/**
 * Find which files could potentially contain the main procedure/function.
 * @param {ResourceList} files list of files to check.
 * @return {Array<string>} the filenames of each potential 'main' file.
 */
export function findMains(files: ResourceList): Array<string> {
  if (files.length == 1) return [files[0].basename];
  const mains: Array<string> = [];
  const fileNames = new Set(files.map((f) => f.basename));
  for (const f of files) {
    if (f.basename.endsWith('.adb')) {
      const expectedAds = f.basename.replace(/.$/, 's');
      if (!fileNames.has(expectedAds)) {
        mains.push(f.basename);
      }
    } else if (
      f.basename.endsWith('.c') &&
      f.contents.search(cMainRegExp) >= 0
    ) {
      mains.push(f.basename);
    }
  }
  return mains;
}

/**
 * Duplicate main finding logic from existing download setup
 * @param {ResourceList} files to be zipped. Used in main finding logic.
 * @param {string} main provided by data-main for the project.
 * @return {string} the file name that contains the main subprogram, with it's
 *                  file extension removed.
 */
export function getMain(files: ResourceList, main: string): string {
  if (main == '') {
    const potentialMains = findMains(files);
    if (potentialMains.length == 1) {
      main = potentialMains[0];
    } else if (potentialMains.length > 1) {
      console.error('More than one main found in project');
      console.error('Generated gpr may be incorrect');
    }
  }
  main = main.split('.')[0];
  if (main == '') return '';
  return `for Main use ("${main}");`;
}

/**
 * Creates the contents of the gpr file for the generated project.
 * @param {ResourceList} files to be zipped. Used in main finding logic.
 * @param {string} switches to be included in the gpr file.
 * @param {string} main provided by data-main for the project.
 * @return {string} the contents of the gpr file to be generated.
 */
export function getGprContents(
    files: ResourceList,
    switches: string,
    main: string
): string {
  const languages = getLanguages(files);
  const parsedSwitches = parseSwitches(switches);
  const newMain = getMain(files, main);
  let gpr = MAIN_GPR;
  gpr = gpr.replace('--MAIN_PLACEHOLDER--', newMain);
  gpr = gpr.replace('--LANGUAGE_PLACEHOLDER--', languages);
  gpr = gpr.replace(
      '--BUILDER_SWITCHES_PLACEHOLDER--',
      parsedSwitches['--BUILDER_SWITCHES_PLACEHOLDER--']
  );
  gpr = gpr.replace(
      '--COMPILER_SWITCHES_PLACEHOLDER--',
      parsedSwitches['--COMPILER_SWITCHES_PLACEHOLDER--']
  );
  return gpr;
}

/**
 * Download a zip of the current Project
 * @param {ResourceList} files to be zipped
 * @param {string} switches to be included in the gpr file
 * @param {string} main provided by data-main for the project
 * @param {string} name of the zip
 */
export function downloadProject(
    files: ResourceList,
    switches: string,
    main: string,
    name: string
): void {
  const zip = new JSZip();

  // Add the source files to the zip
  for (const f of files) {
    zip.file(f.basename, f.contents);
  }

  // Add the ADC to the zip
  zip.file('main.adc', COMMON_ADC);

  // Add the gpr to the zip
  const gpr = getGprContents(files, switches, main);
  zip.file('main.gpr', gpr);

  // Create and download the zip
  zip.generateAsync({type: 'blob'}).then((blob) => {
    FileSaver.saveAs(blob, `${name}.zip`);
  });
}
