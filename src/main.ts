import { NestFactory } from "@nestjs/core";
import { AppModule } from "./app.module";
import { DocumentBuilder, SwaggerModule } from "@nestjs/swagger";
import { PrismaExceptionFilter } from "./prisma/exeptions/prisma-exception.filter";
import { ValidationPipe } from "@nestjs/common/pipes/validation.pipe";

async function bootstrap() {
  const app = await NestFactory.create(AppModule);

  app.useGlobalPipes(
    new ValidationPipe({
      whitelist: true,
      forbidNonWhitelisted: false,
      transform: true,
    }),
  );

  app.enableCors({
    origin: "*",
    secure: false,
    methods: ["GET", "HEAD", "POST", "PUT", "DELETE", "PATCH", "OPTIONS"],
    allowedHeaders: [
      "Content-Type",
      "Accept",
      "Authorization",
      "x-transaction-id",
      "x-tenant-schema",
      "x-tenant-code",
    ],
    credentials: true,
  });

  app.useGlobalFilters(new PrismaExceptionFilter());

  // Configuración de Swagger
  const config = new DocumentBuilder()
    .setTitle("Documentación de la API Inventario")
    .addServer("/api", "API")
    .setDescription("Documentación de la API de desarrollo de Inventario")
    .setVersion("1.0")
    // Agrega, si lo deseas, autenticación u otros detalles
    // .addBearerAuth()
    .build();

  const document = SwaggerModule.createDocument(app, config);
  SwaggerModule.setup("api-docs", app, document);

  await app.listen(process.env.PORT ?? 3000);
}
void bootstrap();
