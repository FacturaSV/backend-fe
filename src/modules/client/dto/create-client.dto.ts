import { ApiProperty, ApiPropertyOptional } from "@nestjs/swagger";
import { EstadoRegistro } from "@prisma/client";
import { Type } from "class-transformer";
import {
  IsEmail,
  IsEnum,
  IsInt,
  IsOptional,
  IsString,
  ValidateNested,
} from "class-validator";

export class ClienteEmpresaInputDto {
  @ApiProperty({
    description: "ID de la empresa relacionada al cliente",
    example: 2,
  })
  @IsInt()
  empresaId: number;
}

export class ClienteActividadEconomicaInputDto {
  @ApiProperty({ description: "ID de la actividad económica", example: 5 })
  @IsInt()
  actividadEconomicaId: number;
}
export class CreateClientDto {
  @ApiProperty({
    description: "Nombre del cliente",
    example: "Yanira Rosibel Palacios",
  })
  @IsString()
  nombre: string;

  @ApiProperty({
    description: "Correo electrónico del cliente",
    example: "usuario5@gmail.com",
  })
  @IsEmail()
  email: string;

  @ApiPropertyOptional({
    description: "Número de teléfono",
    example: "70151139",
  })
  @IsOptional()
  @IsString()
  telefono?: string;

  @ApiProperty({ description: "Tipo de documento", example: "13" })
  @IsString()
  tipoDocumento: string;

  @ApiProperty({ description: "Número de documento", example: "059586029" })
  @IsString()
  numDocumento: string;

  @ApiProperty({
    description: "Dirección del cliente",
    example: "COL LOS ALMENDROS 2 FINAL 3A AVE. SUR CASA 131",
  })
  @IsString()
  direccion: string;

  @ApiPropertyOptional({ description: "NRC del cliente", example: "123456-7" })
  @IsOptional()
  @IsString()
  nrc?: string;

  @ApiPropertyOptional({
    description: "ID del grupo en Keycloak",
    example: "b1f6d842-4eab-4c2f-bb66-ece4b086a49c",
  })
  @IsOptional()
  @IsString()
  keycloakGroupId?: string;

  @ApiProperty({ description: "ID del municipio", example: 176 })
  @IsInt()
  municipioId: number;

  @ApiProperty({ description: "ID del departamento", example: 199 })
  @IsInt()
  departamentoId: number;

  @ApiPropertyOptional({
    description: "Estado del cliente (ACTIVO, INACTIVO, ELIMINADO)",
    example: "ACTIVO",
  })
  @IsOptional()
  @IsEnum(EstadoRegistro)
  estadoRt?: EstadoRegistro;

  @ApiPropertyOptional({
    description: "Actividades económicas asociadas al cliente",
    type: [ClienteActividadEconomicaInputDto],
    example: [{ actividadEconomicaId: 1 }, { actividadEconomicaId: 3 }],
  })
  @IsOptional()
  @ValidateNested({ each: true })
  @Type(() => ClienteActividadEconomicaInputDto)
  ClienteActividadEconomica?: ClienteActividadEconomicaInputDto[];

  @ApiPropertyOptional({
    description: "Empresas asociadas al cliente",
    type: [ClienteEmpresaInputDto],
    example: [{ empresaId: 1 }, { empresaId: 4 }],
  })
  @IsOptional()
  @ValidateNested({ each: true })
  @Type(() => ClienteEmpresaInputDto)
  empresasRegistradas?: ClienteEmpresaInputDto[];
}
