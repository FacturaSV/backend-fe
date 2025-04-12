import { MiddlewareConsumer, Module, NestModule } from '@nestjs/common';
import { SchemaMiddleware } from './middlewares/SchemaMiddleware';
import { PrismaService } from './prisma/prisma.service';
import { SucursalModule } from './modules/sucursal/sucursal.module';
import { HistorialTablasModule } from './modules/historial-tablas/historial-tablas.module';
import { UserModule } from './modules/user/user.module';
import { APP_FILTER } from '@nestjs/core';
import { PrismaExceptionFilter } from './prisma/exeptions/prisma-exception.filter';
import { PrismaExceptionFilterInit } from './prisma/exeptions/prisma-exceptionInit.filter';
import { CategoriaModule } from './modules/categoria/categoria.module';
import { ProductoModule } from './modules/producto/producto.module';
import { ProductoCategoriaModule } from './modules/producto-categoria/producto-categoria.module';
import { ClientModule } from './modules/client/client.module';
import { EmpresaModule } from './modules/empresa/empresa.module';
import { CatalogosModule } from './modules/catalogos/catalogos.module';
import { CronJobModule } from './modules/cron-job/cron-job.module';
import { PrismaModule } from './prisma/prisma.module';
import { DteModule } from './modules/dte/dte.module';

@Module({
  imports: [
    SucursalModule,
    HistorialTablasModule,
    UserModule,
    CategoriaModule,
    ProductoModule,
    ProductoCategoriaModule,
    ClientModule,
    EmpresaModule,
    CatalogosModule,
    CronJobModule,
    PrismaModule,
    DteModule,
  ],
  providers: [
    {
      provide: APP_FILTER,
      useClass: PrismaExceptionFilterInit,
    },
    {
      provide: APP_FILTER,
      useClass: PrismaExceptionFilter,
    },
    PrismaService,
  ],
})
export class AppModule implements NestModule {
  configure(consumer: MiddlewareConsumer) {
    consumer.apply(SchemaMiddleware).forRoutes('*');
  }
}
