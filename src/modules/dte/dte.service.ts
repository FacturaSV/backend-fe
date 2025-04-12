/* eslint-disable @typescript-eslint/no-unsafe-argument */
import {
  BadRequestException,
  Inject,
  Injectable,
  Logger,
} from '@nestjs/common';
import { PrismaService } from 'src/prisma/prisma.service';
import { InfoSucursalDocumento } from './dto/create-dte.dto';
import { CACHE_MANAGER } from '@nestjs/cache-manager';
import type { Cache } from 'cache-manager'; // 👈 importar desde 'cache-manager'
import { Decimal } from '@prisma/client/runtime/library';
import { EstadoRegistro } from '@prisma/client';
import {
  Resumen,
  Pago,
  CuerpoDocumento,
  Receptor,
  FirmarDocumentoRequest,
  FirmarDocumentoResponse,
  EnvioDteRequest,
} from './dto/dte.dto';
import axios, { AxiosError } from 'axios';

@Injectable()
export class DteService {
  private readonly logger = new Logger(DteService.name);

  constructor(
    private readonly prisma: PrismaService,
    @Inject(CACHE_MANAGER) private cacheManager: Cache,
  ) {}

  async firmarDocumento(
    dtePayload: FirmarDocumentoRequest,
  ): Promise<FirmarDocumentoResponse> {
    const url = process.env.FIRMA_HACIENDA_URL || '';
    const headers = {
      'Content-Type': 'application/json',
    };

    try {
      const response = await axios.post(url, dtePayload, { headers });

      return response.data;
    } catch (error) {
      this.logger.error(
        `Error al firmar documento: ${error.message}`,
        error.stack,
      );
      throw error;
    }
  }

  async enviarDocumentoAHacienda(
    uriTrasmision: string,
    payload: EnvioDteRequest,
    token: string,
  ): Promise<any> {
    const headers = {
      'Content-Type': 'application/json',
      Authorization: `${token}`,
    };

    try {
      const response = await axios.post(uriTrasmision, payload, { headers });

      return {
        status: response.status,
        statusText: response.statusText,
        data: response.data,
      };
    } catch (error) {
      const axiosError = error as AxiosError;

      const status = axiosError.response?.status;
      const statusText = axiosError.response?.statusText;
      const data = axiosError.response?.data;

      this.logger.error(
        `Error al enviar DTE a Hacienda. Status: ${status} ${statusText}`,
      );
      if (data) {
        this.logger.error(`Respuesta de error: ${JSON.stringify(data)}`);
      }
      return {
        error: true,
        status,
        statusText,
        data,
      };
    }
  }

  async getTasaIVA(): Promise<number> {
    const cacheKey = 'TASA_IVA';
    const cached = await this.cacheManager.get<number>(cacheKey);

    if (cached !== undefined && cached !== null) {
      return cached;
    }

    const tasa = await this.prisma.tasasVar.findFirst({
      where: {
        abreviatura: 'IVA',
        estadoRt: EstadoRegistro.ACTIVO,
      },
      select: {
        valorNumerico: true,
      },
    });

    const parsed =
      tasa?.valorNumerico instanceof Decimal
        ? tasa.valorNumerico.toNumber()
        : 0.13;

    await this.cacheManager.set(cacheKey, parsed, 60 * 60 * 24);

    return parsed;
  }

  async codigoGeneracion(dte: string, format: string): Promise<string> {
    try {
      const resultado = await this.prisma.$queryRaw<
        { numeroControl: string }[]
      >`
      SELECT facturalink.generate_dte_sequence(${dte}::text, ${format}::text) AS "numeroControl";
    `;

      if (resultado.length > 0) {
        return resultado[0].numeroControl;
      }
      return '';
    } catch (error) {
      console.error('Error al llamar al procedimiento almacenado:', error);
      return '';
    }
  }

  validarPagosResumen(resumen: Resumen, pagos: Pago[]): Resumen {
    const sumaPagos = pagos.reduce((acc, p) => acc + (p.montoPago ?? 0), 0);
    const totalPagado = +sumaPagos.toFixed(2);
    const totalEsperado = +resumen.totalPagar.toFixed(2);

    if (totalPagado !== totalEsperado) {
      throw new BadRequestException(
        `La suma de pagos (${totalPagado}) no coincide con el total a pagar (${totalEsperado})`,
      );
    }

    // TODO: Validar que código y plazo estén dentro del catálogo permitido

    return {
      ...resumen,
      pagos: pagos.map((p) => ({
        ...p,
        montoPago: +p.montoPago.toFixed(2),
      })),
    };
  }

  async informactionDte(
    sucursalId: number,
    codigoDTE: string,
  ): Promise<InfoSucursalDocumento | null> {
    try {
      const result = await this.prisma.$queryRaw<
        { data: InfoSucursalDocumento }[]
      >`
        SELECT facturalink.fn_get_info_sucursal_documento(${sucursalId}::int, ${codigoDTE}::text) AS data;
    `;

      if (result.length > 0 && result[0].data) {
        return result[0].data;
      }

      return null;
    } catch (error) {
      console.error('Error al obtener información del DTE:', error);
      return null;
    }
  }

  async informactionReceptor(
    sucursalId: number,
    isClient: boolean,
    codigoDTE: string,
  ): Promise<Receptor | null> {
    try {
      const result = await this.prisma.$queryRaw<{ data: Receptor }[]>`
        SELECT facturalink.fn_get_info_receptor(${sucursalId}::int, ${isClient}::boolean ,${codigoDTE}::text) AS data;
    `;

      if (result.length > 0 && result[0].data) {
        return result[0].data;
      }

      return null;
    } catch (error) {
      console.error('Error al obtener información del receptor:', error);
      return null;
    }
  }

  calcularIVAItem(item: CuerpoDocumento, tasaIVA: number): number {
    const base = +(item.ventaGravada / (1 + tasaIVA)).toFixed(8);
    return +(item.ventaGravada - base).toFixed(8);
  }

  async validarItem(item: CuerpoDocumento): Promise<CuerpoDocumento> {
    const tasaIVA = await this.getTasaIVA();

    const cantidad = item.cantidad ?? 0;
    const precioUnitario = item.precioUni ?? 0;
    const montoDescuento = item.montoDescu ?? 0;

    const totalBruto = +(cantidad * precioUnitario).toFixed(2);
    const totalNeto = +(totalBruto - montoDescuento).toFixed(2);

    // Inicializar ventas
    let ventaGravada = 0;
    let ventaExenta = 0;
    let ventaNoSuj = 0;

    // Determinar tipo de venta
    if (item.reqInfo?.noIva) {
      // No grava IVA
      ventaExenta = totalNeto;
    } else if (item.ventaNoSuj && item.ventaNoSuj > 0) {
      ventaNoSuj = totalNeto;
    } else if (item.ventaExenta && item.ventaExenta > 0) {
      ventaExenta = totalNeto;
    } else {
      // Por defecto, grava IVA
      ventaGravada = totalNeto;
    }
    item.ventaGravada = this.round(ventaGravada);

    const ivaCalculado = this.calcularIVAItem(item, tasaIVA);

    return {
      ...item,
      ventaGravada,
      ventaExenta,
      ventaNoSuj,
      ivaItem: ivaCalculado,
    };
  }

  round(value: number): number {
    return +value.toFixed(2);
  }

  async calcularResumenConIVA(
    items: CuerpoDocumento[],
    saldoFavor: number = 0,
    descuentoGeneral: number = 0,
  ): Promise<{ cuerpoDocumento: CuerpoDocumento[]; resumen: Resumen }> {
    let totalNoSuj = 0;
    let totalExenta = 0;
    let totalGravada = 0;
    let totalIva = 0;
    let totalNoGravado = 0;

    let descuNoSuj = 0;
    let descuExenta = 0;
    let descuGravada = 0;

    const itemsValidados: CuerpoDocumento[] = [];

    for (const item of items) {
      const itemValidado = await this.validarItem(item);
      itemsValidados.push(itemValidado);

      const ventaGravada = itemValidado.ventaGravada ?? 0;
      const ventaExenta = itemValidado.ventaExenta ?? 0;
      const ventaNoSuj = itemValidado.ventaNoSuj ?? 0;
      const noGravado = itemValidado.noGravado ?? 0;
      const descuento = itemValidado.montoDescu ?? 0;

      totalGravada += ventaGravada;
      totalExenta += ventaExenta;
      totalNoSuj += ventaNoSuj;
      totalNoGravado += noGravado;
      totalIva += itemValidado.ivaItem ?? 0;

      if (descuentoGeneral === 0) {
        if (ventaGravada > 0) descuGravada += descuento;
        if (ventaExenta > 0) descuExenta += descuento;
        if (ventaNoSuj > 0) descuNoSuj += descuento;
      }
    }

    const subTotalVentas = totalGravada + totalExenta + totalNoSuj;
    let totalDescu = 0;

    if (descuentoGeneral > 0) {
      totalDescu = parseFloat(
        (subTotalVentas * (descuentoGeneral / 100)).toFixed(2),
      );
    } else {
      totalDescu = descuGravada + descuExenta + descuNoSuj;
    }

    const subTotal = subTotalVentas - totalDescu;
    const montoTotalOperacion = subTotal;
    const totalPagar = montoTotalOperacion - saldoFavor;

    const porcentajeDescuento =
      subTotalVentas > 0
        ? parseFloat(((totalDescu / subTotalVentas) * 100).toFixed(2))
        : 0;

    const resumen: Resumen = {
      totalNoSuj: parseFloat(totalNoSuj.toFixed(2)),
      totalExenta: parseFloat(totalExenta.toFixed(2)),
      totalGravada: parseFloat(totalGravada.toFixed(2)),
      subTotalVentas: parseFloat(subTotalVentas.toFixed(2)),
      descuNoSuj: parseFloat(descuNoSuj.toFixed(2)),
      descuExenta: parseFloat(descuExenta.toFixed(2)),
      descuGravada: parseFloat(descuGravada.toFixed(2)),
      porcentajeDescuento,
      totalDescu: parseFloat(totalDescu.toFixed(2)),
      tributos: [],
      subTotal: parseFloat(subTotal.toFixed(2)),
      ivaRete1: 0,
      reteRenta: 0,
      montoTotalOperacion: parseFloat(montoTotalOperacion.toFixed(2)),
      totalNoGravado: parseFloat(totalNoGravado.toFixed(2)),
      totalPagar: parseFloat(totalPagar.toFixed(2)),
      totalIva: parseFloat(totalIva.toFixed(2)),
      saldoFavor: parseFloat(saldoFavor.toFixed(2)),
      condicionOperacion: 1,
      totalLetras: this.numeroALetras(totalPagar),
      pagos: null,
      numPagoElectronico: null,
    };

    return {
      cuerpoDocumento: itemsValidados,
      resumen,
    };
  }

  numeroALetras(monto: number): string {
    const unidades = [
      '',
      'UNO',
      'DOS',
      'TRES',
      'CUATRO',
      'CINCO',
      'SEIS',
      'SIETE',
      'OCHO',
      'NUEVE',
      'DIEZ',
      'ONCE',
      'DOCE',
      'TRECE',
      'CATORCE',
      'QUINCE',
      'DIECISÉIS',
      'DIECISIETE',
      'DIECIOCHO',
      'DIECINUEVE',
      'VEINTE',
    ];

    const decenas = [
      '',
      '',
      'VEINTI',
      'TREINTA',
      'CUARENTA',
      'CINCUENTA',
      'SESENTA',
      'SETENTA',
      'OCHENTA',
      'NOVENTA',
    ];

    const centenas = [
      '',
      'CIENTO',
      'DOSCIENTOS',
      'TRESCIENTOS',
      'CUATROCIENTOS',
      'QUINIENTOS',
      'SEISCIENTOS',
      'SETECIENTOS',
      'OCHOCIENTOS',
      'NOVECIENTOS',
    ];

    function convertir(num: number): string {
      if (num === 0) return 'CERO';
      if (num === 100) return 'CIEN';
      if (num < 21) return unidades[num];
      if (num < 100) {
        const unidad = num % 10;
        const decena = Math.floor(num / 10);
        return unidad === 0
          ? decenas[decena]
          : decenas[decena] + ' Y ' + unidades[unidad];
      }
      if (num < 1000) {
        const resto = num % 100;
        const centena = Math.floor(num / 100);
        return centenas[centena] + (resto > 0 ? ' ' + convertir(resto) : '');
      }
      if (num < 1000000) {
        const miles = Math.floor(num / 1000);
        const resto = num % 1000;
        const milesTexto = miles === 1 ? 'MIL' : convertir(miles) + ' MIL';
        return milesTexto + (resto > 0 ? ' ' + convertir(resto) : '');
      }

      return 'UN MILLÓN O MÁS';
    }

    const enteros = Math.floor(monto);
    const centavos = Math.round((monto - enteros) * 100);

    const letras = `${convertir(enteros)} ${centavos
      .toString()
      .padStart(2, '0')}/100 DÓLARES`;

    return letras;
  }
  fixDecimal(value: number | null | undefined): number {
    return value != null ? parseFloat(value.toFixed(2)) : 0;
  }

  formatearDtePayload(payload: any): any {
    // Formatear cuerpoDocumento
    if (payload.cuerpoDocumento && Array.isArray(payload.cuerpoDocumento)) {
      payload.cuerpoDocumento = payload.cuerpoDocumento.map((item) => ({
        ...item,
        precioUni: this.fixDecimal(item.precioUni),
        montoDescu: this.fixDecimal(item.montoDescu),
        ventaNoSuj: this.fixDecimal(item.ventaNoSuj),
        ventaExenta: this.fixDecimal(item.ventaExenta),
        ventaGravada: this.fixDecimal(item.ventaGravada),
        psv: this.fixDecimal(item.psv),
        noGravado: this.fixDecimal(item.noGravado),
        ivaItem: this.fixDecimal(item.ivaItem),
      }));
    }

    // Formatear resumen
    if (payload.resumen) {
      const r = payload.resumen;
      r.totalNoSuj = this.fixDecimal(r.totalNoSuj);
      r.totalExenta = this.fixDecimal(r.totalExenta);
      r.totalGravada = this.fixDecimal(r.totalGravada);
      r.subTotalVentas = this.fixDecimal(r.subTotalVentas);
      r.descuNoSuj = this.fixDecimal(r.descuNoSuj);
      r.descuExenta = this.fixDecimal(r.descuExenta);
      r.descuGravada = this.fixDecimal(r.descuGravada);
      r.totalDescu = this.fixDecimal(r.totalDescu);
      r.subTotal = this.fixDecimal(r.subTotal);
      r.ivaRete1 = this.fixDecimal(r.ivaRete1);
      r.reteRenta = this.fixDecimal(r.reteRenta);
      r.montoTotalOperacion = this.fixDecimal(r.montoTotalOperacion);
      r.totalNoGravado = this.fixDecimal(r.totalNoGravado);
      r.totalPagar = this.fixDecimal(r.totalPagar);
      r.totalIva = this.fixDecimal(r.totalIva);
      r.saldoFavor = this.fixDecimal(r.saldoFavor);

      // Formatear pagos si existen
      if (r.pagos && Array.isArray(r.pagos)) {
        r.pagos = r.pagos.map((p) => ({
          ...p,
          montoPago: this.fixDecimal(p.montoPago),
        }));
      }
    }

    return payload;
  }
}
