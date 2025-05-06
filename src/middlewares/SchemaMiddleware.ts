import { Injectable, NestMiddleware } from "@nestjs/common";
import { Request, Response, NextFunction } from "express";
import * as jwt from "jsonwebtoken"; // Si necesitas decodificar el token

@Injectable()
export class SchemaMiddleware implements NestMiddleware {
  use(req: Request, res: Response, next: NextFunction) {
    try {
      const authHeader = req.headers.authorization;
      if (!authHeader) {
        return next();
      }

      const token = authHeader.split(" ")[1]; // asumiendo "Bearer <token>"
      if (!token || typeof token !== "string") {
        return next();
      }

      const decoded = jwt.decode(token) as Record<string, any>;

      const domainFromToken = decoded?.domain || "core";

      const codeFromToken = decoded?.domain_cod || "core";

      // Almacenas el schema en el request con una propiedad custom:
      req["tenant-schema"] = domainFromToken;
      // req['historial-active'] = true;

      req.headers["x-tenant-schema"] = domainFromToken;
      req.headers["x-tenant-code"] = codeFromToken;
      req.headers["x-user"] = "1";
    } catch (error) {
      console.error("Error setting schema:", error);
    }
    next();
  }
}
