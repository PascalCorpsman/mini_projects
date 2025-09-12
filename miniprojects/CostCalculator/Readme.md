# Cost calculator

I am a owner of a photovoltaic system and was curious how effective the system is. If you have all data of consumption and generation you can play with the numbers to see different impact values ;).


The application expects the following "topology":

![](topo.png)

A House including a battery and photovoltaic system connected over a measuring device to the public current system. 

At least in germany this topology makes sense, as the price for "selling" energy to the public is way less than "buying" from the public.

| Value | Description |
| --- | --- |
| Cost per kWh | the amount you have to pay for consuming 1 kWh from the public net
| Profit per kWh | the amount you get payed for "selling" 1 kWh to the public net
| Akkusize in kWh | the size of the battery
| from Net [MWh] | the amount of energy which actually was consumed by your house from the net
| Akku->House [MWh] | the amount of energy which was consumed from akku to house
| Sun->House [MWh] | the amount of energy which was directly feed from the sun to the house (attention, this value is twice on generating and consuption side)
| to Net [MWh] | the amount of energy which was sold to the net (not used)
| Sun->Akku [MWh] | the amount of energy that was used to recharge the akku

![](preview.png)

Features:
- adjustable akku capacity
- adjustable costs
