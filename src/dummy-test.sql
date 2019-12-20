--     quotationDatabase(filtered: $filtered) {
--       margin {
--         production
--         transport
--         handling
--       }
--       plantList {
--         id
--         productRangeList {
--           id
--           price
--           product {
--             id
--             isSaturday
--             class {
--               id
--               name
--             }
--           }
--         }
--         truckList {
--           id
--           equipment {
--             mixer {
--               capacity
--             }
--             pipe {
--               length
--             }
--             pump {
--               length
--             }
--             carpet {
--               minLength
--               maxLength
--             }
--             crane {
--               capacity
--             }
--           }
--           signature
--         }
--         plantCostList {
--         	id
--           name
--           truckIdList
--           isForTruck
--           cost {
--           	variable
--             costRangeList {
--               domain
--               rate
--               flat
--             }
--           }
--         }
--       }
--     }
--   }




select 
    row(
        plant.id,
        plant.organization_id,
        (
            select array_agg(row(product_range.id, product_range.price))
            from product_range
            where product_range.plant_id=plant.id AND product_range.deleted_at IS NULL
            group by product_range.plant_id
        ),
        (
            select array_agg(distinct row(label.id, label.name))
            from label
            JOIN label_on ON label.id=label_on.label_id
            where label_on.plant_id=plant.id
            group by label_on.plant_id
        ),
        (
            select array_agg(row(truck.id, truck.equipment))
            from truck
            join truck_plant ON truck_plant.plant_id=plant.id AND truck_plant.truck_id=truck.id
            group by truck_plant.plant_id
        )
    ),
    count(*) over ()
from plant
join truck_plant ON truck_plant.plant_id=plant.id
join truck ON truck.id=truck_plant.truck_id
WHERE ((truck).equipment).pump IS NOT NULL
group by plant.id




-------------------------------------------------------------------------------------------------------------------------------
select 
    row(
        person.id,
        (
            select array_agg(row(organization.id, organization.name))
            from organization
            JOIN belonging ON belonging.id=person.belonging_id
            where belonging.organization_id=organization.id
            group by person.id
        ),
        (
            select array_agg(row(
                    work.id,
                    (
                        select row(
                            quotation.id,
                            (
                                select array_agg(distinct case when ((truck).equipment).pump IS NOT NULL THEN 'pompe' else 'ok' end)
                                from truck
                                join truck_projection ON truck_projection.truck_id=truck.id
                                join projection on projection.id=truck_projection.projection_id
                                where projection.quotation_id=quotation.id
                                group by quotation.id
                            )
                        )
                        from quotation
                        where quotation.work_id=work.id
                        group by quotation.id
                        limit 1
                    )
                ))
            from work
            where work.person_id=person.id
            group by work.person_id
        )
    )
from person
group by person.id
limit 10;
-------------------------------------------------------------------------------------------------------------------------------
select row(
        work.id,
        (
            select row(
                quotation.id,
                (
                    select array_agg(distinct case when ((truck).equipment).pump IS NOT NULL THEN 'pompe' else 'ok' end)
                    from truck
                    join truck_projection ON truck_projection.truck_id=truck.id
                    join projection on projection.id=truck_projection.projection_id
                    where projection.quotation_id=quotation.id
                    group by quotation.id
                )
            )
            from quotation
            where quotation.work_id=work.id
            group by quotation.id
            limit 1
        )
    )
from work
limit 10
-------------------------------------------------------------------------------------------------------------------------------
select row(
        work.id,
        (
            select row(
                quotation.id,
                (
                    select array_agg(distinct case when ((truck).equipment).pump IS NOT NULL THEN 'pompe' else 'ok' end)
                    from truck
                    join truck_projection ON truck_projection.truck_id=truck.id
                    join projection on projection.id=truck_projection.projection_id
                    where projection.quotation_id=quotation.id
                    group by quotation.id
                )
            )
            from quotation
            where quotation.work_id=work.id
            group by quotation.id
            limit 1
        ),
        (
            select row(work_schedule.id, work_schedule.pouring_date)
            from work_schedule
            where work.id=work_schedule.work_id
            group by work_schedule.work_id, work_schedule.id, work_schedule.pouring_date, work_schedule.created_at
            order by work_schedule.created_at desc
            limit 1
        )
        -- work_projection ? only show the current active order ? 
        -- (
        --     select row(work_schedule.id, work_schedule.pouring_date)
        --     from projection
        --     join work_projection ON work_projection.work_id=work.id
        --     where work.id=work_schedule.work_id
        --     group by work_schedule.work_id, work_schedule.id, work_schedule.pouring_date, work_schedule.created_at
        --     order by work_schedule.created_at desc
        --     limit 1
        -- )
    )
from work
join work_schedule_view ON work.id=work_schedule_view.work_id
join work_projection ON work.id=work_projection.work_id
where work_projection.code in ('ORDER', 'MODIF', 'MODIFTIME') and work_projection.deleted_id IS NULL
group by work.id, work_schedule_view.pouring_date, work_projection.delivery_time
order by work_schedule_view.pouring_date asc, work_projection.delivery_time asc
limit 20


