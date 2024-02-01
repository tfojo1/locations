# Test plot: State CBSAs
state.cbsa = get.contained.locations("TX","CBSA")
state2.cbsa = get.contained.locations("OH", "CBSA")
# name_data = c(names(state.cbsa),names(state2.cbsa))
state_data = c("MD","MI","WA")
code_data = c(unname(state.cbsa),unname(state2.cbsa), state_data)

state.df = data.frame(locations=code_data, size=rep(1,length(code_data)), color=rev(seq(1,length(code_data))))

location.plot(state.df,aes(x=longitude, y=latitude, size=size, color=color, fill=color), "State CBSAs", alpha=0.4)
