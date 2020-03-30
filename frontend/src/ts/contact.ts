import {fetchJSON} from './comms';

type ValidationFunction = (data: string) => boolean;

interface ValidationObjects {
  object: JQuery;
  validateFn: ValidationFunction;
}

interface SubmissionReturn {
  success: boolean;
}

/** The Contact Form class */
export class ContactForm {
  private container: JQuery;
  private form: JQuery;
  private readonly loader: JQuery;
  private readonly server: string;

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
      this.loader.appendTo(this.container);
      this.form.fadeOut(200);

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
        const formData = this.form.serializeArray();
        try {
          const ret =
            await fetchJSON<Array<JQuery.NameValuePair>,
              SubmissionReturn>(formData, this.server + '/contact_form/');
          if (ret.success) {
            // TODO: figure this out
            console.log('Form submission success');
          } else {
            throw new Error('Failed to submit form.');
          }
        } catch (error) {
          // TODO: figure out error handling
          console.log(error);
          this.form.show();
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
