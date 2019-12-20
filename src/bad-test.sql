-- NON
select row(organization.id, array_agg(row(plant.id, array_agg(row(truck_plant.truck_id)))))
from organization
join plant on plant.organization_id=plant.id
join truck_plant ON truck_plant.plant_id = plant.id

-- TEST 1
-- depth == 1 => normal join/left join
select 
    row(
        organization.id,
        array_agg(
            row(
                plant.id,
                (
                    select array_agg(row(truck.id))
                    from truck_plant
                    join truck ON truck.id=truck_plant.truck_id
                    where truck_plant.plant_id=plant.id
                    group by truck_plant.plant_id
                )
            )
        )
    )
from organization
join plant on plant.organization_id=organization.id
group by organization.id
offset 10
limit 20


-- TEST AA
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
            having count(label_on.label_id) % 2 = 0
        )
    ),
    count(*) over ()
from plant
join label_on ON label_on.plant_id=plant.id AND label_on.label_id='686677c1-5209-46fa-a9cf-c7aa92f4647d'
group by plant.id
having count(label_on.label_id) % 2 = 0

-- TEST 2
select row(organization.id, array_agg(plant_list))
from organization
join (
    select plant.id, array_agg(row(truck_plant.truck_id)) as truck_list, organization_id
    from plant
    join truck_plant ON truck_plant.plant_id = plant.id
    group by plant.id
) as plant_list(id, truck_list, organization_id) on plant_list.organization_id=organization.id
group by organization.id

-- TEST 3
select row(organization.id, array_agg(plant_list))
from organization
join (
    select plant.id, array_agg(row(truck_plant.truck_id)) as truck_list, organization_id
    from plant
    join truck_plant ON truck_plant.plant_id = plant.id
    group by plant.id
    order by plant.created_at
    limit 1
) as plant_list on plant_list.organization_id=organization.id
where organization.id='e66c07a5-5b8c-45e4-8dc1-af19b8fdd78d'
group by organization.id

-- TEST 4
with plant_list as (
    select plant.id, array_agg(row(truck_plant.truck_id)) as truck_list, organization_id
    from plant
    join truck_plant ON truck_plant.plant_id = plant.id
    group by plant.id
    order by plant.created_at
    limit 1
)
select row(organization.id, array_agg(plant_list))
from organization
join plant_list on plant_list.organization_id=organization.id
where organization.id='e66c07a5-5b8c-45e4-8dc1-af19b8fdd78d'
group by organization.id


