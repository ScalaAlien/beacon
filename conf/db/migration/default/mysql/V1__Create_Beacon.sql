CREATE TABLE `beacon` (
  `id`                             BIGINT AUTO_INCREMENT NOT NULL,
  `serial`                         CHAR(9)               NOT NULL,
  `ble_address`                    CHAR(12)              NOT NULL,
  `ng`                             SMALLINT(6)           NOT NULL,
  `finished_product_inspection_at` TIMESTAMP             NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `packaging_at`                   TIMESTAMP             NULL     DEFAULT NULL,
  `visual_inspection_defective_at` TIMESTAMP             NULL     DEFAULT NULL,
  `create_at`                      TIMESTAMP             NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `update_at`                      TIMESTAMP             NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`),
  UNIQUE (`serial`, `ble_address`)
)
  ENGINE = InnoDB;