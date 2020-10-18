
library(tidyverse)
library(data.table)
library(gtable)
library(dplyr)
library(lubridate)

Historical_Shipments %>%
  mutate(X1_cost = ifelse(`Str Nbr` == "1" & Supplier == "A",
                        2540.00,
                        NA))

Historical_Shipments2 <- Historical_Shipments


shipments.by.rates <- left_join(Historical_Shipments2,
         Carrier_Rates)

orders.by.shipments <- left_join(Historical_Orders,
                                 Historical_Shipments2,
                                 by = c("Str Nbr",
                                        "Supplier",
                                        "Order Dt" = "Shipment Date"))

master <- left_join(orders.by.shipments,
                    shipments.by.rates,
                    by = c("Str Nbr",
                           "Supplier",
                           "Order Dt" = "Shipment Date"))

final.master <- master %>%
  group_by(week(`Order Dt`),
           `Str Nbr`) %>%
  mutate(hammer_orders = ifelse(`Product Id` == 1,
                                `Order Qty` * 1.1,
                                0)) %>%
  mutate(real_hammer_orders = max(hammer_orders)) %>%
  group_by(Carrier) %>%
  mutate(total_weight = 2 * real_hammer_orders + Weight.x) %>%
  mutate(ship_cost = ifelse(Carrier == "X", Cost, Cost * real_hammer_orders * 2 + Cost * Weight.x)) %>%
  mutate(product_cost = ifelse(Supplier == "A",
                             .80 * real_hammer_orders,
                             .82 * real_hammer_orders))

final.master %>%
  arrange(-total_weight) %>%
  view(final.master)

view(final.master)

#Annual Shipping Cost A
final.master %>%
  filter(Supplier == "A") %>%
  group_by(week(`Order Dt`),
           `Str Nbr`) %>%
  mutate(min_ship_cost = min(ship_cost)) %>%
  ungroup() %>%
  group_by(week(`Order Dt`)) %>%
  mutate(total_weekly_ship_cost = max(min_ship_cost)+min(min_ship_cost)) %>%
  ungroup() %>%
  summarize(annual_ship_cost = sum(total_weekly_ship_cost/8))

#Annual product cost A
final.master %>%
  filter(Supplier == "A") %>%
  ungroup() %>%
  summarize(annual_product_cost = sum(product_cost/4))
  
  
#Annual shipping cost B
final.master %>%
  filter(Supplier == "B") %>%
  group_by(week(`Order Dt`),
           `Str Nbr`) %>%
  mutate(min_ship_cost = min(ship_cost)) %>%
  ungroup() %>%
  group_by(week(`Order Dt`)) %>%
  mutate(total_weekly_ship_cost = max(min_ship_cost)+min(min_ship_cost)) %>%
  ungroup() %>%
  summarize(annual_ship_cost = sum(total_weekly_ship_cost/4))


#Annual product cost B
final.master %>%
  filter(Supplier == "B") %>%
  ungroup() %>%
  summarize(annual_product_cost = sum(product_cost/2))



master %>%
  group_by(week(`Order Dt`)) %>%
  filter(`Product Id` == 1) %>%
  mutate(hammer_orders = sum(`Order Qty`*1.1)/2) %>%
  mutate(hammer_weight = 2 * hammer_orders) %>%
  mutate(hammer_ship_cost_A = ifelse(Carrier == "X", Cost, Cost * hammer_weight)) %>%
  mutate(hammer_ship_cost_B = ifelse(Carrier == "X", 1200, Cost * hammer_weight)) %>%
  mutate(hammer_product_cost_A = ifelse(Supplier == "A", .80 * hammer_orders, .82 * hammer_orders)) %>%
  mutate(hammer_product_cost_B = hammer_product_cost_A * 1.025) %>%
  mutate(total_hammer_cost_A = hammer_ship_cost_A + hammer_product_cost_A) %>%
  mutate(total_hammer_cost_B = hammer_ship_cost_B + hammer_product_cost_B) %>%
  view(master)



master.w.hammer <- master %>%
  mutate(ship_cost = ifelse(Carrier == "X",
                            Cost,
                            Cost * `Order Weight`)) %>%
  mutate(order_weight_hammer = ifelse((Carrier == "Y" | Carrier == "X") & `Product Id` == 1,
                                      10/11 * `Order Weight` + `Order Weight`,
                                      `Order Weight`)) %>%
  mutate(ship_cost_hammer = ifelse(Carrier == "Y" & `Product Id` == 1,
                                   Cost * order_weight_hammer,
                                   ship_cost)) %>%
  view(master.w.hammer)





master.w.hammer %>%
  group_by(week(`Order Dt`),
                `Str Nbr`,
                Supplier) %>%
  ggplot(aes(x = week(`Order Dt`),
             y = ship_cost_hammer)) +
  geom_point(aes(color = Carrier))







