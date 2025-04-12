**Comandos de Prisma en NestJS**

### **1. Instalación de Prisma**

```sh
npm install @prisma/client
npm install -D prisma
```

### **2. Inicializar Prisma en el proyecto**

```sh
npx prisma init
```

_Crea la carpeta `prisma/` con el archivo `schema.prisma`._

---

### **3. Generar el Cliente de Prisma**

```sh
npx prisma generate
```

_Se debe ejecutar cada vez que se actualiza el `schema.prisma`._

---

### **4. Migraciones (Modificar la base de datos)**

#### **Crear una nueva migración**

```sh
npx prisma migrate dev --name nombre_migracion
```

_Aplica los cambios en la base de datos y crea el historial de migraciones._

#### **Sincronizar el esquema sin historial de migraciones**

```sh
npx prisma db push
```

_Sincroniza el `schema.prisma` con la base de datos sin crear migraciones._

#### **Revertir la última migración**

```sh
npx prisma migrate reset
```

_Esto eliminará todos los datos y aplicará las migraciones desde cero._

---

### **5. Consultar la base de datos**

#### **Abrir Prisma Studio (interfaz visual para ver los datos)**

```sh
npx prisma studio
```

#### **Ejecutar consultas SQL directamente**

```sh
npx prisma db execute --file=archivo.sql --url=DATABASE_URL
```

---

### **6. Exportar o importar datos**

#### **Obtener el esquema de la base de datos actual**

```sh
npx prisma db pull
```

_Sincroniza `schema.prisma` con la estructura de la base de datos._

#### **Sembrar datos (seeding)**

Si tienes un archivo `prisma/seed.ts`, ejecuta:

```sh
npx prisma db seed
```

---

### **7. Depuración y logs**

#### **Ver el estado de las migraciones**

```sh
npx prisma migrate status
```

#### **Activar logs detallados en consultas**

```sh
PRISMA_LOG_LEVEL=debug npx prisma generate
```

---

### **8. Eliminar y recrear la base de datos**

#### **Borrar y volver a crear la base de datos**

```sh
npx prisma migrate reset
```

_⚠️ Elimina todos los datos y vuelve a aplicar las migraciones._

---

### **9. Comandos Avanzados**

#### **Ver modelos en la base de datos**

```sh
npx prisma format
```

#### **Ver la versión de Prisma instalada**

```sh
npx prisma -v
```

#### **Generar archivos de cliente Prisma manualmente**

```sh
npx prisma generate
```

---

🚀 **Con estos comandos puedes gestionar completamente Prisma en NestJS!**
