import { Module } from "@nestjs/common";
import { ClientService } from "./client.service";
import { ClientController } from "./client.controller";
import { EmpresaService } from "../empresa/empresa.service";

@Module({
  controllers: [ClientController],
  providers: [ClientService, EmpresaService],
})
export class ClientModule {}
