export interface DtePayload {
  identificacion: Identificacion;
  documentoRelacionado: any; // puede ser null o un objeto, puedes ajustar
  emisor: Emisor;
  receptor: Receptor;
  otrosDocumentos: any;
  ventaTercero: any;
  cuerpoDocumento: CuerpoDocumento[];
  resumen: Resumen;
  extension: any;
  apendice: any;
}

export interface Identificacion {
  version: number;
  ambiente: string;
  tipoDte: string;
  numeroControl: string;
  codigoGeneracion: string;
  tipoModelo: number;
  tipoOperacion: number;
  tipoContingencia: string | null;
  motivoContin: string | null;
  fecEmi: string;
  horEmi: string;
  tipoMoneda: string;
}

export interface Emisor {
  nit: string;
  nrc: string;
  nombre: string;
  codActividad: string | null;
  descActividad: string | null;
  nombreComercial: string;
  tipoEstablecimiento: string;
  direccion: Direccion;
  telefono: string;
  correo: string;
  codEstableMH: string;
  codEstable: string;
  codPuntoVentaMH: string | null;
  codPuntoVenta: string | null;
}

export interface Receptor {
  tipoDocumento: string;
  numDocumento: string;
  nrc: string | null;
  nombre: string;
  codActividad: string | null;
  descActividad: string | null;
  direccion: Direccion;
  telefono: string;
  correo: string;
}

export interface Direccion {
  departamento: string;
  municipio: string;
  complemento: string;
}

export interface CuerpoDocumento {
  numItem: number;
  tipoItem: number;
  numeroDocumento: string | null;
  cantidad: number;
  codigo: string;
  codTributo: string | null;
  uniMedida: number;
  descripcion: string;
  precioUni: number;
  montoDescu: number;
  ventaNoSuj: number;
  ventaExenta: number;
  ventaGravada: number;
  tributos: string[] | null;
  psv: number;
  noGravado: number;
  ivaItem?: number;
  reqInfo?: {
    isAlcohol?: boolean;
    isGun?: boolean;
    noIva?: boolean;
    hasDiscount?: boolean;
    discountValue?: number;
  };
}

export interface Resumen {
  totalNoSuj: number;
  totalExenta: number;
  totalGravada: number;
  subTotalVentas: number;
  descuNoSuj: number;
  descuExenta: number;
  descuGravada: number;
  porcentajeDescuento: number;
  totalDescu: number;
  tributos: any[]; // puedes detallar si tienes estructura
  subTotal: number;
  ivaRete1: number;
  reteRenta: number;
  montoTotalOperacion: number;
  totalNoGravado: number;
  totalPagar: number;
  totalLetras: string;
  totalIva: number;
  saldoFavor: number;
  condicionOperacion: number;
  pagos?: Pago[] | null;
  numPagoElectronico: string | null;
}

export interface Pago {
  codigo: string;
  montoPago: number;
  plazo: string;
  referencia: string;
  periodo: string | null;
}

export interface FirmarDocumentoRequest {
  nit: string;
  activo: boolean;
  passwordPri: string;
  dteJson: any;
}

export interface FirmarDocumentoResponse {
  status: string;
  body: string; // Este es un JWT en formato string
}

export interface EnvioDteRequest {
  ambiente: string;
  idEnvio: number;
  version: number;
  tipoDte: string;
  documento: string; // JWT del documento firmado
  codigoGeneracion: string;
}

export type DteCreditoFiscalPayload = DtePayload;
