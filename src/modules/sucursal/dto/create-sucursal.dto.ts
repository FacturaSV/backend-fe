import { ApiProperty, ApiPropertyOptional } from "@nestjs/swagger";
import {
  IsEmail,
  IsEnum,
  IsNumber,
  IsOptional,
  IsString,
} from "class-validator";

// Enum de estado
export enum EstadoRegistro {
  ACTIVO = "ACTIVO",
  INACTIVO = "INACTIVO",
  ELIMINADO = "ELIMINADO",
}

export class CreateSucursalDto {
  @ApiProperty({
    example: "Sucursal Central",
    description: "Nombre de la sucursal",
  })
  @IsString()
  nombre: string;

  @ApiPropertyOptional({
    example: "Av. Principal 123",
    description: "Dirección de la sucursal",
  })
  @IsOptional()
  @IsString()
  direccion?: string;

  @ApiPropertyOptional({
    example: "+503 7123-4567",
    description: "Teléfono de la sucursal",
  })
  @IsOptional()
  @IsString()
  telefono?: string;

  @ApiPropertyOptional({
    example: "contacto@sucursal.com",
    description: "Correo electrónico de la sucursal",
  })
  @IsOptional()
  @IsEmail()
  email?: string;

  @ApiPropertyOptional({
    example: "001",
    description: "Código de punto de venta",
  })
  @IsOptional()
  @IsString()
  codPuntoVenta?: string;

  @ApiPropertyOptional({
    example: "12345678",
    description: "Código de punto de venta (Ministerio de Hacienda)",
  })
  @IsOptional()
  @IsString()
  codPuntoVentaMH?: string;

  @ApiPropertyOptional({ example: "DTE-001", description: "Código DTE único" })
  @IsOptional()
  @IsString()
  codigoDte?: string;

  @ApiPropertyOptional({
    example: "001",
    description: "Código de establecimiento",
  })
  @IsOptional()
  @IsString()
  codEstable?: string;

  @ApiPropertyOptional({
    example: "0001",
    description: "Código de establecimiento (MH)",
  })
  @IsOptional()
  @IsString()
  codEstableMH?: string;

  @ApiPropertyOptional({
    example: "01",
    description: "Tipo de establecimiento (Emisor)",
    enum: ["01", "02", "04", "07", "20"],
  })
  @IsOptional()
  @IsString()
  tipoEstablecimiento?: string;

  @ApiPropertyOptional({ example: 12, description: "ID del municipio" })
  @IsOptional()
  @IsNumber()
  municipioId?: number;

  @ApiPropertyOptional({ example: 5, description: "ID del departamento" })
  @IsOptional()
  @IsNumber()
  departamentoId?: number;

  @ApiPropertyOptional({
    example: EstadoRegistro.ACTIVO,
    description: "Estado del registro",
    enum: EstadoRegistro,
  })
  @IsOptional()
  @IsEnum(EstadoRegistro)
  estadoRt?: EstadoRegistro;

  @ApiPropertyOptional({
    example: 1,
    description: "ID del usuario que crea la sucursal",
  })
  @IsOptional()
  @IsNumber()
  createdBy?: number;

  @ApiPropertyOptional({
    example: 1,
    description: "ID del usuario que actualiza la sucursal",
  })
  @IsOptional()
  @IsNumber()
  updatedBy?: number;

  @ApiPropertyOptional({
    example: 1,
    description: "ID del usuario que elimina la sucursal",
  })
  @IsOptional()
  @IsNumber()
  deletedBy?: number;

  @ApiPropertyOptional({ example: 3, description: "ID de la empresa" })
  @IsOptional()
  @IsNumber()
  empresaId?: number;
}
