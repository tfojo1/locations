# Test plot: State CBSAs
#state2.cbsa = LOCATION.MANAGER$get.overlapping("MT", "CBSA", TRUE)
state2.cbsa = c()
state.counties = c(LOCATION.MANAGER$get.contained("RI", "COUNTY", TRUE))
# state.counties = c(LOCATION.MANAGER$get.contained("AK","COUNTY", TRUE))
# name_data = c(names(state.cbsa),names(state2.cbsa))
state.data = c()
#state.data = c("MD","MI","WA")
# code_data = c(unname(state2.cbsa), state_data, unname(state.counties))
code_data = c(unname(state2.cbsa), state.data, unname(state.counties))

print(state.counties)

state.df = data.frame(locations=code_data, size=rep(1,length(code_data)), color=c(1,5,5,1,5))

location.plot(state.df,color="color",fill="color", title="Testing", bb="AUTO", alpha=0.4)
# location.plot(state.df,color="color",fill="color", title="Testing", alpha=0.4)

