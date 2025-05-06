export interface AnularDteDto {
    nit: string;
    activo: boolean;
    passwordPri: string;
    dteJson: DteJsonDto;
  }

  export interface AnularDTERequest{
    anularDteDto: AnularDteDto,
    empresaInfo: IEmpresa
  }

  export interface IEmpresa{
    sucursarId: number;
    codigoDTE: string; 
  }
  
  export interface DteJsonDto {
    identificacion: IdentificacionDto;
    emisor: EmisorDto;
    documento: DocumentoDto;
    motivo: MotivoDto;
  }
  
  export interface IdentificacionDto {
    version: number;
    ambiente: string;
    codigoGeneracion: string;
    fecAnula: string;
    horAnula: string;
  }
  
  export interface EmisorDto {
    nit: string;
    nombre: string;
    tipoEstablecimiento: string;
    nomEstablecimiento?: string | null;
    telefono?: string | null;
    correo?: string | null;
    codPuntoVentaMH?: string | null;
    codEstableMH?: string | null;
    codPuntoVenta?: string | null;
  }
  
  export interface DocumentoDto {
    tipoDte: string;
    codigoGeneracionR?: string | null;
    codigoGeneracion: string;
    selloRecibido: string;
    numeroControl: string;
    fecEmi: string;
    montoIva?: number;
    tipoDocumento?: string;
    numDocumento?: string;
    nombre?: string;
    telefono?: string;
    correo?: string;

  }
  
  export interface MotivoDto {
    tipoAnulacion: number;
    motivoAnulacion: string;
    nombreResponsable: string;
    tipDocResponsable: string;
    numDocResponsable: string;
    nombreSolicita: string;
    tipDocSolicita: string;
    numDocSolicita: string;
  }
  