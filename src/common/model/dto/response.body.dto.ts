export class ResponseDto<T> {
  code: number;
  message: string;
  status: string;
  data: T;

  constructor(code: number, message: string, status: string, data: T) {
    this.code = code;
    this.message = message;
    this.status = status;
    this.data = data;
  }
}
