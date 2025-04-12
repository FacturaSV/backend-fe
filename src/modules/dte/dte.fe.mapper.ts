import { Injectable } from '@nestjs/common';
import { DateTime } from 'luxon';
import {
  DtePayload,
  Identificacion,
  Emisor,
  Receptor,
  CuerpoDocumento,
  Resumen,
  Direccion,
} from './dto/dte.dto';
import { DocumentoInfo, InfoSucursalDocumento } from './dto/create-dte.dto';

@Injectable()
export class DteFeMapper {
  constructor() {}

  buildIdentificacion(
    tipoDte: string,
    numeroControl: string,
    codigoGeneracion: string,
    ambiente: string,
  ): Identificacion {
    const now = DateTime.now().setZone('America/El_Salvador');

    const fecEmi = now.toFormat('yyyy-MM-dd');
    const horEmi = now.toFormat('HH:mm:ss');

    return {
      version: 1,
      ambiente: ambiente,
      tipoDte,
      numeroControl,
      codigoGeneracion,
      tipoModelo: 1,
      tipoOperacion: 1,
      tipoContingencia: null,
      motivoContin: null,
      fecEmi,
      horEmi,
      tipoMoneda: 'USD',
    };
  }

  buildEmisor(info: InfoSucursalDocumento, actividadEconomica: string): Emisor {
    const suc = info.sucursal;

    const actividad = info.empresa.actividadesEconomicas.find(
      (item) => item.codigo === actividadEconomica,
    );

    return {
      nit: suc.nit,
      nrc: suc.nrc,
      nombre: suc.nombre,
      codActividad: actividad?.codigo ?? null,
      descActividad: actividad?.valor ?? null,
      nombreComercial: suc.nombreComercial,
      tipoEstablecimiento: suc.tipoEstablecimiento,
      direccion: this.mapDireccion(suc.direccion),
      telefono: suc.telefono,
      correo: suc.correo,
      codEstableMH: suc.codEstableMH,
      codEstable: suc.codEstable,
      codPuntoVentaMH: suc.codPuntoVentaMH,
      codPuntoVenta: suc.codPuntoVenta,
    };
  }

  buildReceptor(cliente: {
    tipoDocumento: string;
    numDocumento: string;
    nrc: string | null;
    nombre: string;
    codActividad: string | null;
    descActividad: string | null;
    direccion: Direccion;
    telefono: string;
    correo: string;
  }): Receptor {
    return {
      tipoDocumento: cliente.tipoDocumento,
      numDocumento: cliente.numDocumento,
      nrc: cliente.nrc,
      nombre: cliente.nombre,
      codActividad: cliente.codActividad,
      descActividad: cliente.descActividad,
      direccion: this.mapDireccion(cliente.direccion),
      telefono: cliente.telefono,
      correo: cliente.correo,
    };
  }

  mapDireccion(direccion: Direccion): Direccion {
    return {
      departamento: direccion.departamento,
      municipio: direccion.municipio,
      complemento: direccion.complemento,
    };
  }

  buildCuerpoDocumento(items: CuerpoDocumento[]): CuerpoDocumento[] {
    return items;
  }

  buildResumen(resumen: Resumen): Resumen {
    return resumen;
  }

  buildPayload(
    documento: DocumentoInfo,
    numeroControl: string,
    codigoGeneracion: string,
    emisorInfo: InfoSucursalDocumento,
    receptor: Receptor,
    cuerpoDocumento: CuerpoDocumento[],
    resumen: Resumen,
    codigoActividadEco: string,
  ): DtePayload {
    return {
      identificacion: this.buildIdentificacion(
        documento.codigo,
        numeroControl,
        codigoGeneracion,
        documento.ambiente.codigo,
      ),
      documentoRelacionado: null,
      emisor: this.buildEmisor(emisorInfo, codigoActividadEco),
      receptor,
      otrosDocumentos: null,
      ventaTercero: null,
      cuerpoDocumento: this.buildCuerpoDocumento(cuerpoDocumento),
      resumen: this.buildResumen(resumen),
      extension: null,
      apendice: null,
    };
  }
}
