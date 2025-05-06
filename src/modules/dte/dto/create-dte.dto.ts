import { CuerpoDocumento, Pago } from './dte.dto';

export interface CreateDteFEdto extends ValidateTotalesDto {
  sucursarId: number;
  codigoActEconomicaR: string;
  codigoActEconomicaE: string;
  codigoDTE: string;
  isBusiness: boolean;
  receptorId: number;
  isClient: boolean;
  pagos: Pago[];
}

export interface ValidateTotalesDto {
  items: CuerpoDocumento[];
  descuentoGeneral: number;
  saldoFavor: number;
}

export interface InfoSucursalDocumento {
  empresa: EmpresaInfo;
  documento: DocumentoInfo;
  credencial: CredencialInfo;
  sucursal: SucursalInfo;
}

export interface EmpresaInfo {
  id: number;
  nombre: string;
  nit: string;
  nrc: string;
  telefono: string;
  correo: string;
  logo: string;
  actividadesEconomicas: ActividadesEconomicas[];
}

export interface ActividadesEconomicas {
  codigo: string;
  valor: string;
}

export interface DocumentoInfo {
  id: number;
  codigo: string;
  valor: string;
  abreviatura: string;
  ambiente: AmbienteInfo;
}

export interface AmbienteInfo {
  id: number;
  codigo: string;
  valor: string;
  descripcion: string;
}

export interface CredencialInfo {
  id: number;
  usuario: string;
  password: string;
  clavePublica: string;
  clavePrivada: string;
  nombreCliente: string;
  accessToken: string;
  uriRecepcion: string;
  uriAnulacion: string;
  uriAuth: string;
  uriContigencia: string;
}

export interface SucursalInfo {
  nit: string;
  nrc: string;
  nombre: string;
  codActividad: string | null;
  descActividad: string | null;
  nombreComercial: string;
  tipoEstablecimiento: string;
  direccion: DireccionInfo;
  telefono: string;
  correo: string;
  codigoDte: string;
  codEstableMH: string;
  codEstable: string;
  codPuntoVentaMH: string | null;
  codPuntoVenta: string | null;
}

export interface DireccionInfo {
  departamento: string;
  municipio: string;
  complemento: string;
}

export interface CreateDteCreditoFiscalDto {
  sucursarId: number;
  codigoDTE: string; // Debe ser "03" para CCF
  codigoActEconomicaR: string; // Actividad económica del receptor
  codigoActEconomicaE: string; // Actividad económica del emisor
  receptorId: number;
  isClient: boolean;
  isBusiness: boolean;
  items: CuerpoDocumento[];
  pagos: Pago[];
  descuentoGeneral: number;
  saldoFavor: number;
}