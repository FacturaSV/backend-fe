import { Controller, Post, Body, Headers, HttpException, HttpStatus, Logger } from '@nestjs/common';
import { DteService } from './dte.service';
import { TrxDteService } from './trx-dte.service';
import { DteFeMapper } from './dte.fe.mapper';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';
import { DteCreditoFiscalPayload } from './dto/dte.dto';
import { CreateDteCreditoFiscalDto } from './dto/create-dte.dto';


@Controller('dte-ccf')
export class DteCcfController {
  private DOCUMENTO = 'Comprobante de Crédito Fiscal';
  private readonly logger = new Logger(DteCcfController.name);

  constructor(
    private readonly dteService: DteService,
    private readonly trxDteService: TrxDteService,
    private readonly dteMapper: DteFeMapper, // por ahora, reutilizamos el mismo mapper
  ) {}

  @Post('/generate')
  async generateCcf(
    @Body() request: CreateDteCreditoFiscalDto,
    @Headers('x-transaction-id') transactionId?: string,
    @Headers('x-tenant-schema') tenantSchema?: string,
  ): Promise<ResponseDto<any>> {
    this.logger.log(`Transaction ID: ${transactionId} - Generando ${this.DOCUMENTO}`);

    // Paso 1: Calcular totales con IVA
    const totals = await this.dteService.calcularResumenConIVA(
      request.items,
      request.saldoFavor,
      request.descuentoGeneral,
    );

    // Paso 2: Validar pagos
    const resumen = this.dteService.validarPagosResumen(
      totals.resumen,
      request.pagos,
    );

    // Paso 3: Obtener datos del emisor
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

    // Paso 4: Generar número de control
    const numeroControl = await this.dteService.codigoGeneracion(
      emisorInfo.documento.codigo,
      emisorInfo.sucursal.codigoDte,
    );

    // Paso 5: Generar UUID
    const codigoGeneracion = crypto.randomUUID().toUpperCase();

    // Paso 6: Obtener info del receptor
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

    // Paso 7: Armar el payload DTE (Crédito Fiscal)
    const payload: DteCreditoFiscalPayload = this.dteMapper.buildPayload(
      emisorInfo.documento,
      numeroControl,
      codigoGeneracion,
      emisorInfo,
      receptor,
      totals.cuerpoDocumento,
      resumen,
      request.codigoActEconomicaE,
    );

    // this.logger.log(
    //   `Transaction ID: ${transactionId} - Payload de ${this.DOCUMENTO} armado correctamente`,
    // );

    this.logger.debug(
      `🖨️ Payload JSON generado:\n${JSON.stringify(payload, null, 2)}`,
    );
    

    // // Paso 8: Firmar documento
    const documento = await this.dteService.firmarDocumento({
      nit: emisorInfo.empresa.nit,
      activo: true,
      passwordPri: emisorInfo.credencial.clavePrivada,
      dteJson: payload,
    });

    // Paso 9: Enviar a Hacienda
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

    // Paso 10: Guardar en base de datos
    const finalJson = {
      ...payload,
      firmaElectronica: documento.body,
      recepcionMh: hacienda.data,
    };

    const dteCcf = await this.trxDteService.createTrxDte({
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

    return new ResponseDto(
      200,
      `${this.DOCUMENTO} generado correctamente`,
      'success',
      finalJson,
    );
  }

}
