import { Controller, Post, Body, Headers, Logger, BadRequestException } from '@nestjs/common';
import { ResponseDto } from 'src/common/model/dto/response.body.dto';
import { AnularDteDto, AnularDTERequest } from './dto/anular-dte.dto';
import { DteService } from './dte.service';

@Controller('dte-anular')
export class DteAnularController {
  private readonly logger = new Logger(DteAnularController.name);

  constructor(private readonly dteService: DteService) {}
// Promise<ResponseDto<any>>
  @Post('/anular')
  async anularDte(
    @Body() request: AnularDTERequest,
    @Headers('authorization') token: string,
  ): Promise<ResponseDto<any>> {
    this.logger.log('Anulando DTE...');
  
    // Aquí llamas al servicio para construir el request completo
    const anularRequest = await this.dteService.construirAnularDteRequest(request.anularDteDto, request.empresaInfo);
    this.logger.log('Imprimiendo json' + JSON.stringify(anularRequest, null, 2));

    const firmado = await this.dteService.firmarDocumento(anularRequest);

    if (firmado.status === 'ERROR') {
      this.logger.error('Error firmando documento: ' + JSON.stringify(firmado.body));
      throw new BadRequestException('No se pudo firmar el DTE');
    }

    this.logger.log('Documento firmado correctamente. documento: ' + JSON.stringify(firmado));


    const payloadAnular = {
        ambiente: anularRequest.dteJson.identificacion.ambiente,
        idEnvio: Math.floor(Math.random() * 1000000), 
        version: anularRequest.dteJson.identificacion.version,
        documento: firmado.body, 
      };

      const response = await this.dteService.enviarDocumentoAHacienda(
        'https://apitest.dtes.mh.gob.sv/fesv/anulardte',
        payloadAnular,
        token,
      );
    
      return new ResponseDto(200, 'Anulación enviada a Hacienda', 'success', response);
  }
}
