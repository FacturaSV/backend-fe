export enum EstadoTranDTE {
  PROCESADO = 'PROCESADO',
  FALLIDO = 'FALLIDO',
}

export enum EnvioPdfDTE {
  ENVIADO = 'ENVIADO',
  PENDIENTE = 'PENDIENTE',
}
export interface TransaccionesDteDto {
  version: number;
  tipoDte: string;
  numeroControl: string;
  codigoGeneracion: string;
  tipoModelo: number;
  tipoOperacion: number;
  tipoContingencia?: string | null;
  motivoContin?: string | null;
  fhProcesamiento: Date; // o Date si lo convertís directamente
  tipoMoneda: string;
  cantIntento: number;
  dteJson: any; // Podés tipar esto como tu `DteJson` si tenés una interfaz para ello

  descripcionMsg?: string | null;
  selloRecibido?: string | null;

  estadoTranDTE?: EstadoTranDTE;
  envioPdfDTE?: EnvioPdfDTE;

  documentosDteId?: number | null;
}
