/* eslint-disable @typescript-eslint/no-unsafe-argument */
export function generatePrismaSelect(
  dto: any,
  excludeFields: string[] = [],
): Record<string, boolean> {
  const instance = new dto();
  const properties = Object.keys(instance); // Obtener propiedades del DTO

  return properties.reduce(
    (acc, prop) => {
      if (!excludeFields.includes(prop)) {
        acc[prop] = true; // Solo incluir propiedades permitidas
      }
      return acc;
    },
    {} as Record<string, boolean>,
  );
}
