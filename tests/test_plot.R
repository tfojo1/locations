# Test plot: State CBSAs
#state2.cbsa = get.overlapping.locations("MT", "CBSA")
state2.cbsa = c()
state.counties = c(get.contained.locations("NV", "COUNTY"))
# state.counties = c(get.contained.locations("AK","COUNTY"))
# name_data = c(names(state.cbsa),names(state2.cbsa))
state.data = c()
#state.data = c("MD","MI","WA")
# code_data = c(unname(state2.cbsa), state_data, unname(state.counties))
code_data = c(unname(state2.cbsa), state.data, unname(state.counties))
print(code_data)


state.df = data.frame(locations=code_data, size=rep(1,length(code_data)), color=rev(seq(1,length(code_data))))

location.plot(state.df,color="color",fill="color", title="Testing", bb="AUTO", alpha=0.4)
