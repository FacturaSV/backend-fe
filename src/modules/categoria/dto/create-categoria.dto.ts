import { ApiProperty, ApiPropertyOptional } from "@nestjs/swagger";
import { IsOptional, IsString, IsNumber } from "class-validator";

export class CreateCategoriaDto {
  @ApiProperty({
    description: "Nombre de la categoría",
    example: "Lácteos",
  })
  @IsString()
  nombre: string;

  @ApiProperty({
    description: "Nivel de la categoría (1 para categorías principales)",
    example: "Lácteos",
  })
  @IsNumber()
  nivel: number;

  @ApiPropertyOptional({
    description: "Descripción de la categoría",
    example: "Productos derivados de la leche",
  })
  @IsOptional()
  @IsString()
  descripcion?: string;

  @ApiPropertyOptional({
    description: "ID de la categoría padre (para subcategorías)",
    example: 1,
  })
  @IsOptional()
  @IsNumber()
  categoriaPadreId?: number;

  @ApiPropertyOptional({
    description: "Empresa a la que pertenece la categoría",
    example: "1",
  })
  @IsOptional()
  @IsString()
  empresaId?: number;
}
