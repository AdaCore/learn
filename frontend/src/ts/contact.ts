import {fetchJSON} from './comms';
import {ActionState, Button, CheckBox} from './components';
import * as Strings from './strings';

interface SubmitRequest {
  Name: string;
  Email: string;
  Message: string;
  GDPRConsent: boolean;
}

interface SubmitResponse {
  success: boolean;
}

enum TagType {
  Input = 'input',
  TextArea = 'textarea',
}

/** Abstract base class for Form fields */
abstract class Field {
  protected input: HTMLInputElement | HTMLTextAreaElement;
  protected label: HTMLLabelElement;
  private readonly errorLabel: HTMLSpanElement;
  public name: string;

  /**
   * Constructs a Form Field
   * @param {HTMLFormElement} container - The parent container
   * @param {string} type - The type of input ['text', 'email', etc]
   * @param {string} name - The name to give the label, also the tooltip
   * @param {string} id - The id to give the label, and the tag
   * @param {string} tag - The tag for the input
   */
  constructor(container: HTMLFormElement,
      type: string,
      name: string,
      id: string,
      tag = TagType.Input) {
    this.name = name;

    this.label = document.createElement('label');
    this.label.setAttribute('for', id);
    this.label.textContent = name;
    container.appendChild(this.label);

    this.input = document.createElement(tag);
    this.input.classList.add('field');
    this.input.setAttribute('type', type);
    this.input.setAttribute('id', id);
    this.input.setAttribute('name', name);
    container.appendChild(this.input);

    this.errorLabel = document.createElement('span');
    this.errorLabel.classList.add('form-error');
    this.errorLabel.textContent = this.getErrorText();
  }

  /**
   * The abstract validation function
   * @abstract
   * @return boolean
   */
  abstract validate(): boolean;

  /**
   * Return the error text to display to the user for this field
   * @abstract
   * @returns {string}
   */
  abstract getErrorText(): string;

  /**
   * Returns the value in the field as a string
   * @return {string}
   */
  public getVal(): string {
    return this.input.value;
  }

  /**
   * Sets the value in the field as a string
   * @param {string} val - the value to set in the field
   */
  public setVal(val: string): void {
    this.input.value = val;
  }

  /**
   * Resets the field
   */
  public reset(): void {
    this.input.classList.remove('form-error');
    this.errorLabel.remove();
  }

  /**
   * Adds error classes to the field
   */
  public addError(): void {
    this.input.classList.add('form-error');
    this.label.insertAdjacentElement('afterend', this.errorLabel);
  }

  /**
   * Adds the required class to the field
   */
  public addRequired(): void {
    this.label.classList.add('required');
    this.input.classList.add('required');
  }

  /**
   * The event callback signature for form fields
   *
   * @callback eventCallback
   * @param {Event} event - The event that triggered the call
   */

  /**
   * Registers an event on the form field
   * @param {string} type - The event type to register ["click", "focus", etc]
   * @param {eventCallback} fn - The callback to trigger
   */
  public registerEvent(type: string, fn: (event: Event) => void): void {
    this.input.addEventListener(type, (event: Event) => {
      const valid = this.validate();
      this.reset();
      if (!valid) {
        this.addError();
      }
      fn(event);
    });
  }
}

/** A Form text field
 * @extends Field
 */
class TextField extends Field {
  /**
   * Constructs a form text field
   * @param {HTMLFormElement} container - The parent container
   * @param {string} name - The name for the label and tooltip
   * @param {string} id - The id for the label and input tag
   */
  constructor(container: HTMLFormElement, name: string, id: string) {
    super(container, 'text', name, id);
  }

  /**
   * The function to validate the input
   * @return {boolean}
   */
  public validate(): boolean {
    return ((this.input.value).length > 0);
  }

  /**
   * Return the error text for this field
   * @return {string}
   */
  public getErrorText(): string {
    return Strings.FORM_NAME_ERROR_TEXT;
  }
}

/**
 * A Form Email field
 * @extends Field
 */
class EmailField extends Field {
  /**
   * Constructs a Form Email Field
   * @param {HTMLFormElement} container
   * @param {string} id - The id for the label and input tag
   */
  constructor(container: HTMLFormElement, id: string) {
    super(container, 'email', 'Email', id);
  }

  /**
   * Validates email field
   * @return {boolean}
   */
  public validate(): boolean {
    const emailRegex = /^\w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+$/;
    return emailRegex.test((this.input.value));
  }

  /**
   * Return the error text for this field
   * @return {string}
   */
  public getErrorText(): string {
    return Strings.FORM_EMAIL_ERROR_TEXT;
  }
}

/**
 * A Form Text area
 * @extends Field
 */
class TextArea extends Field {
  /**
   * Creates an instance of text area.
   * @param {HTMLFormElement} container - The parent object
   * @param {string} name - The label value and tooltip
   * @param {string} id - The id for the label and textarea tag
   */
  constructor(container: HTMLFormElement, name: string, id: string) {
    super(container, 'text', name, id, TagType.TextArea);
  }

  /**
   * Validates the Text Area
   * @return {boolean}
   */
  public validate(): boolean {
    return ((this.input.value).length > 0);
  }

  /**
   * Return the error text for this field
   * @return {string}
   */
  public getErrorText(): string {
    return Strings.FORM_MESSAGE_ERROR_TEXT;
  }
}

/** The Contact Form class */
export class ContactForm {
  private container: HTMLDivElement;
  private form: HTMLFormElement;
  private readonly spinner: HTMLDivElement;
  private readonly server: string;
  private gdprConsent: CheckBox;
  private submitButton: Button;

  private readonly failDOM: HTMLDivElement;
  private readonly successDOM: HTMLDivElement;

  private fieldList: Array<Field> = [];

  /**
   * Constructs the ContactForm
   * @param {HTMLDivElement} container - the container for the contact form
   * @param {string} server - the server address:port
   */
  constructor(container: HTMLDivElement, server: string) {
    this.container = container;
    this.server = server;

    const findForms = this.container.querySelectorAll('form');
    if (findForms.length != 1) {
      throw Error('Malformed contact form! HTML error.');
    }

    this.form = findForms[0];

    this.failDOM = document.createElement('div');
    this.failDOM.classList.add('form-fail');
    this.failDOM.innerHTML = '<p>' + Strings.FORM_FAIL + '</p>';

    this.successDOM = document.createElement('div');
    this.successDOM.classList.add('form-success');
    this.successDOM.innerHTML = '<p>' + Strings.FORM_SUCCESS + '</p>';

    this.spinner = document.createElement('div');
    this.spinner.classList.add('spinner');
    for (let i = 1; i < 4; i++) {
      const b = document.createElement('div');
      b.classList.add('bounce' + i);
      this.spinner.appendChild(b);
    }

    const nameField = new TextField(this.form, 'Name', 'name-field');
    nameField.addRequired();
    nameField.registerEvent('focusout', () => {
      this.submitProtector();
    });
    this.fieldList.push(nameField);

    const emailField = new EmailField(this.form, 'email-field');
    emailField.addRequired();
    emailField.registerEvent('focusout', () => {
      this.submitProtector();
    });
    this.fieldList.push(emailField);

    const messageField = new TextArea(this.form, 'Message', 'message-field');
    messageField.addRequired();
    messageField.registerEvent('focusout', () => {
      this.submitProtector();
    });
    this.fieldList.push(messageField);

    const privPolicy = document.createElement('div');
    privPolicy.innerHTML = '<p>' + Strings.FORM_PRIVACY_POLICY + '</p>';
    this.form.appendChild(privPolicy);

    this.submitButton = new Button(['form-submit'], 'Submit', 'Submit');
    this.gdprConsent = new CheckBox(Strings.FORM_GDPR_CONSENT, undefined,
        ['form-checkbox', 'required'], 'Privacy Policy');
    this.gdprConsent.registerEvent('change', () => {
      this.submitProtector();
    });

    this.form.appendChild(this.gdprConsent.render());

    this.submitButton.registerEvent('click', async () => {
      if (this.submitButton.getActionState() == ActionState.Enabled) {
        this.submitButton.disable();
        this.successDOM.remove();
        this.failDOM.remove();

        const formData: SubmitRequest = {
          Name: '',
          Email: '',
          Message: '',
          GDPRConsent: false,
        };
        for (const field of this.fieldList) {
          field.reset();
          formData[field.name] = field.getVal();
        }

        formData['GDPRConsent'] = this.gdprConsent.checked();
        this.container.appendChild(this.spinner);

        try {
          const ret =
            await fetchJSON<SubmitRequest,
              SubmitResponse>(formData, this.server + '/contact_form/');
          if (ret.success) {
            this.container.appendChild(this.successDOM);
            for (const field of this.fieldList) {
              field.setVal('');
            }
            this.gdprConsent.setChecked(false);
          } else {
            throw new Error('Failed to submit form.');
          }
        } catch (error) {
          this.submitButton.enable();
          this.container.appendChild(this.failDOM);
        } finally {
          this.container.removeChild(this.spinner);
        }
      }
    });
    this.submitButton.disable();
    this.form.appendChild(this.submitButton.render());
  }

  /**
   * Validates the form
   * @return {boolean} True for valid
   */
  private validate(): boolean {
    let ret = true;

    for (const field of this.fieldList) {
      if (!field.validate()) {
        ret = false;
      }
    }

    return ret && this.gdprConsent.checked();
  }

  /**
   * Protects the submit button
   */
  private submitProtector(): void {
    if (this.validate()) {
      this.submitButton.enable();
    } else {
      this.submitButton.disable();
    }
  }
}
/**
 * Entrypoint for contact form creation
 *
 * @export
 * @param {HTMLCollectionOf<Element>} forms - The collection of contact forms
 *     found on the page. This is the return value of getElementsByClass
 * @return {Array<ContactForm>} The list of contact forms on the page
 */
export function contactFactory(forms: HTMLCollectionOf<Element>):
    Array<ContactForm> {
  const formList = [];
  for (let i = 0; i < forms.length; i++) {
    const element = (forms[i] as HTMLDivElement);
    const server = element.getAttribute('example_server');
    try {
      if (server) {
        formList.push(new ContactForm(element, server));
      } else {
        throw Error('Malformed contact form! No server address specified.');
      }
    } catch (error) {
      // an error has occured parsing the contact form
      console.error('Error:', error);

      // clear the offending element to remove any processing that was done
      element.innerHTML = '';

      // add an error message to the page in its place
      const errorDiv = document.createElement('div');
      errorDiv.innerHTML = '<p>An error has occured processing this form. ' +
      Strings.INTERNAL_ERROR_MESSAGE + '</p>';

      element.appendChild(errorDiv);
    }
  }

  return formList;
}
