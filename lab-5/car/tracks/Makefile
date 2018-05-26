NUMS = 1 2 3 6 8 9

tasks = $(addsuffix .txt, $(addprefix task, $(NUMS)))
policies = $(addprefix policy_for_, $(tasks))
CAR=stack exec -- car-exe


all: $(policies) policy_for_task10.txt policy_for_task11.txt

policy_for_task10.txt : task10.txt
	$(CAR) --if $< --of $@ -n 2000

policy_for_task11.txt : task11.txt
	$(CAR) --if $< --of $@ -n 2000

$(policies) : policy_for_%: %
	$(CAR) --if $< --of $@ -n 300