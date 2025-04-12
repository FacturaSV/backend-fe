import {
  Controller,
  Post,
  Body,
  Headers,
  Logger,
  HttpException,
  HttpStatus,
} from '@nestjs/common';
import { DteService } from './dte.service';
import {
  CreateDteFEdto as createDteDto,
  ValidateTotalesDto,
} from './dto/create-dte.dto';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';
import { DtePayload } from './dto/dte.dto';
import { DteFeMapper } from './dte.fe.mapper';
import { TrxDteService } from './trx-dte.service';
import { json } from 'stream/consumers';

@Controller('dte-fe')
export class DteFeController {
  private DOCUMENTO = 'Factura Electronica';
  private readonly logger = new Logger(DteFeController.name);

  constructor(
    private readonly dteService: DteService,
    private readonly trxDteService: TrxDteService,
    private readonly dteMapper: DteFeMapper,
  ) {}

  @Post('/generate')
  async generate(
    @Body() request: createDteDto,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    this.logger.log(
      `Transaction ID: ${transactionId} - Generando ${this.DOCUMENTO}`,
    );

    // 1. Calcular totales
    const totals = await this.dteService.calcularResumenConIVA(
      request.items,
      request.saldoFavor,
      request.descuentoGeneral,
    );

    // 2. Validar pagos
    const resumen = this.dteService.validarPagosResumen(
      totals.resumen,
      request.pagos,
    );

    // 3. Obtener datos del emisor (sucursal, empresa, credenciales, etc.)
    const emisorInfo = await this.dteService.informactionDte(
      request.sucursarId,
      request.codigoDTE,
    );

    if (!emisorInfo) {
      throw new HttpException(
        new ResponseDto(
          400,
          'Error al obtener la información del emisor',
          'error',
          null,
        ),
        HttpStatus.BAD_REQUEST,
      );
    }

    // 4. Generar código de control
    const numeroControl = await this.dteService.codigoGeneracion(
      emisorInfo.documento.codigo,
      emisorInfo.sucursal.codigoDte,
    );

    // 5. Generar UUID
    const codigoGeneracion = crypto.randomUUID().toUpperCase();

    const receptor = await this.dteService.informactionReceptor(
      request.receptorId,
      request.isClient,
      request.codigoActEconomicaR,
    );

    if (!receptor) {
      throw new HttpException(
        new ResponseDto(
          400,
          'Error al obtener la información del receptor',
          'error',
          null,
        ),
        HttpStatus.BAD_REQUEST,
      );
    }

    // 7. Construir el payload usando el mapper
    const payload: DtePayload = this.dteMapper.buildPayload(
      emisorInfo.documento,
      numeroControl,
      codigoGeneracion,
      emisorInfo,
      receptor,
      totals.cuerpoDocumento,
      resumen,
      request.codigoActEconomicaE,
    );

    this.logger.log(
      `Transaction ID: ${transactionId} - ${this.DOCUMENTO} generado correctamente`,
    );

    const documento = await this.dteService.firmarDocumento({
      nit: emisorInfo.empresa.nit,
      activo: false,
      passwordPri: emisorInfo.credencial.clavePrivada,
      dteJson: payload,
    });

    const hacienda = await this.dteService.enviarDocumentoAHacienda(
      emisorInfo.credencial.uriRecepcion,
      {
        ambiente: emisorInfo.documento.ambiente.codigo,
        idEnvio: 1,
        version: 1,
        tipoDte: payload.identificacion.tipoDte,
        codigoGeneracion: payload.identificacion.codigoGeneracion,
        documento: documento.body,
      },
      emisorInfo.credencial.accessToken,
    );

    const finalJson = {
      ...payload,
      firmaElectronica: documento.body,
      recepcionMh: hacienda.data,
    };

    const dteFE = await this.trxDteService.createTrxDte({
      version: finalJson.recepcionMh.version,
      tipoDte: finalJson.identificacion.tipoDte,
      numeroControl: finalJson.identificacion.numeroControl,
      codigoGeneracion: finalJson.identificacion.codigoGeneracion,
      tipoModelo: finalJson.identificacion.tipoModelo,
      tipoOperacion: finalJson.identificacion.tipoOperacion,
      tipoContingencia: finalJson.identificacion.tipoContingencia,
      motivoContin: finalJson.identificacion.motivoContin,
      fhProcesamiento: new Date(finalJson.recepcionMh.fhProcesamiento),
      tipoMoneda: finalJson.identificacion.tipoMoneda,
      cantIntento: 0,
      dteJson: finalJson,
      descripcionMsg: finalJson.recepcionMh.descripcionMsg,
      selloRecibido: finalJson.recepcionMh.selloRecibido,
    });
    this.logger.log(
      `Transaction ID: ${transactionId} - generado correctamente`,
      dteFE,
    );

    return new ResponseDto(
      200,
      `${this.DOCUMENTO} generado correctamente`,
      'success',
      {
        ...payload,
        firmaElectronica: documento.body,
        recepcionMh: hacienda.data,
      },
    );
  }

  @Post('/totales')
  async calculate(
    @Body() request: ValidateTotalesDto,
    @Headers('x-transaction-id') transactionId?: string,
  ): Promise<ResponseDto<any>> {
    try {
      this.logger.log(
        `Transaction ID: ${transactionId} - Calculando totales para el documento ${this.DOCUMENTO}`,
      );

      if (!request?.items || !Array.isArray(request.items)) {
        throw new Error('La lista de items es inválida o no proporcionada');
      }

      const totals = await this.dteService.calcularResumenConIVA(
        request.items,
        request.saldoFavor,
        request.descuentoGeneral,
      );

      this.logger.log(
        `Transaction ID: ${transactionId} - Totales calculados para el documento ${this.DOCUMENTO}`,
      );

      return new ResponseDto(
        200,
        `Información de ${this.DOCUMENTO} procesada correctamente`,
        'success',
        totals,
      );
    } catch (error) {
      this.logger.error(
        `Error calculando totales para ${this.DOCUMENTO}:`,
        error.stack || error.message,
      );

      throw new HttpException(
        new ResponseDto(
          400,
          'Error al calcular los totales del documento',
          'error',
          error.message || null,
        ),
        HttpStatus.BAD_REQUEST,
      );
    }
  }
}
