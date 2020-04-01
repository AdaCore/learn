import {fetchJSON} from './comms';
import * as Strings from './strings';

type ValidationFunction = (data: string) => boolean;

interface ValidationObjects {
  object: JQuery;
  validateFn: ValidationFunction;
}

interface SubmitRequest {
  Name: string;
  Email: string;
  Message: string;
}

interface SubmitResponse {
  success: boolean;
}

/**
 * Convert a JQuery NameValuePair to SubmitForm Interface
 * @param {JQuery.NameValuePair} json - the data to convert
 * @return {SubmitRequest} the converted SubmitForm interface
 */
function convertNVP(json: Array<JQuery.NameValuePair>): SubmitRequest {
  const ret: SubmitRequest = {
    Name: '',
    Email: '',
    Message: '',
  };

  for (const field of json) {
    ret[field['name']] = field['value'];
  }
  return ret;
}

/** The Contact Form class */
export class ContactForm {
  private container: JQuery;
  private form: JQuery;
  private readonly loader: JQuery;
  private readonly server: string;

  private failDOM: JQuery= $('<span>')
      .addClass('form-fail')
      .text(Strings.FORM_FAIL);
  private successDOM: JQuery = $('<span>')
      .addClass('form-success')
      .text(Strings.FORM_SUCCESS);

  private validateList: Array<ValidationObjects> = [];

  /**
   * Constructs the ContactForm
   * @param {JQuery} container - the container for the contact form
   * @param {string} server - the server address:port
   */
  constructor(container: JQuery, server: string) {
    this.container = container;
    this.server = server;

    this.form = this.container.find('form');

    this.loader = $('<div>')
        .addClass('lds-ring');
    for (let i = 0; i < 4; i++) {
      this.loader.append(
          $('<div>')
      );
    }

    this.form.children().each(
        (index: number, element: HTMLElement) => {
          if (!$(element).is('label')) {
            const vType = $(element).attr('type');
            switch (vType) {
              case 'email': {
                this.registerValidator($(element), (data: string): boolean => {
                  const emailRegex =
                    /^\w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+$/;
                  return emailRegex.test(data);
                });
                break;
              }
              case 'text': {
                this.registerValidator($(element), (data: string): boolean => {
                  return data.length != 0;
                });
                break;
              }
              case 'submit': {
                $('label[for="' + $(element).attr('name') + '"]').remove();
                $(element)
                    .addClass('btn')
                    .addClass('btn-primary');
                break;
              }
            }
          }
        });

    this.form.on('submit', async (event: JQuery.SubmitEvent) => {
      event.preventDefault();
      this.successDOM.remove();
      this.failDOM.remove();
      let allGood = true;

      for (const field of this.validateList) {
        const fieldVal = field.object.val() as string;
        field.object.removeClass('form-error');
        if (!field.validateFn(fieldVal)) {
          allGood = false;
          field.object.addClass('form-error');
        }
      }
      if (allGood) {
        this.form.fadeOut(200, () => {
          this.loader.appendTo(this.container);
        });

        const formData = this.form.serializeArray();
        const msgData = convertNVP(formData);

        try {
          const ret =
            await fetchJSON<SubmitRequest,
              SubmitResponse>(msgData, this.server + '/contact_form/');
          if (ret.success) {
            this.loader.fadeOut(200, () => {
              this.successDOM.appendTo(this.container);
            });
          } else {
            throw new Error('Failed to submit form.');
          }
        } catch (error) {
          this.form.fadeIn(200, () => {
            this.loader.fadeOut(200);
            this.failDOM.appendTo(this.container);
          });
        } finally {
          this.loader.remove();
        }
      }
    });
  }

  /**
   * Register a validation function
   * @param {JQuery} element - the element to validate
   * @param {ValidationFunction} vFn - the function to call
   */
  private registerValidator(element: JQuery, vFn: ValidationFunction): void {
    this.validateList.push({
      object: element,
      validateFn: vFn,
    });
  }
}
