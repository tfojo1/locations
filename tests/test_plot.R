# Test plot: State CBSAs
state2.cbsa = get.overlapping.locations("MT", "CBSA")
state.counties = c(get.contained.locations("CA","COUNTY"))
# name_data = c(names(state.cbsa),names(state2.cbsa))
state.data = c("MD","MI","WA")
# code_data = c(unname(state2.cbsa), state_data, unname(state.counties))
code_data = c(unname(state2.cbsa), state.data, unname(state.counties))

state.df = data.frame(locations=code_data, size=rep(1,length(code_data)), color=rev(seq(1,length(code_data))))

location.plot(state.df,size="size",color="color",fill="color", "Testing", alpha=0.4)

