
CREATE TABLE IF NOT EXISTS "plant" (
  "id" UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  "name" TEXT NOT NULL DEFAULT '',
  "createdAt" TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TYPE chassis_type AS (
    wheels       int,
    motricity    int
);

CREATE TABLE IF NOT EXISTS "truck" (
  "id" UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  "chassis" chassis_type NOT NULL DEFAULT row(-1, -1),
  "vehicle_id" TEXT NOT NULL DEFAULT '',
  "createdAt" TIMESTAMP NOT NULL DEFAULT now()
);

CREATE TABLE IF NOT EXISTS "truck_plant" (
  "id" UUID PRIMARY KEY DEFAULT gen_random_uuid(),
  "plant_id" UUID NOT NULL REFERENCES "plant"("id"),
  "truck_id" UUID NOT NULL REFERENCES "truck"("id"),
  "createdAt" TIMESTAMP NOT NULL DEFAULT now()
);
