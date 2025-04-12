import { Injectable } from '@nestjs/common';
import {
  ActividadEconomica,
  CatalogosAgrupados,
  Credenciales,
} from '@prisma/client';
import { PrismaService } from 'src/prisma/prisma.service';

@Injectable()
export class CatalogosService {
  constructor(private readonly prisma: PrismaService) {}

  async findAllCatalogos(): Promise<CatalogosAgrupados[]> {
    return await this.prisma.catalogosAgrupados.findMany();
  }

  async findAllActEconomica(): Promise<ActividadEconomica[]> {
    return await this.prisma.actividadEconomica.findMany();
  }

  async findDteInfo(): Promise<CatalogosAgrupados[]> {
    return await this.prisma.catalogosAgrupados.findMany();
  }

  async findAllCredential(): Promise<Credenciales[]> {
    return await this.prisma.credenciales.findMany();
  }
}
