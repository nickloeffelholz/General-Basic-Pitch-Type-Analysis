wOBAbyLaunch<-SEC2019%>%
  group_by(cut(SEC2019$Angle,breaks=200))%>%
  summarize(BIP=sum(PlayResult!="Undefined"),Launch=mean(Angle,na.rm=TRUE),wOBA=(((.870*(sum(PlayResult=="Single")))+(1.217*(sum(PlayResult=="Double")))+(1.529*(sum(PlayResult=="Triple")))+(1.940*(sum(PlayResult=="HomeRun")))))/(BIP))
plot(wOBAbyLaunch$Launch,wOBAbyLaunch$wOBA)
'Lots of noise on extreme launch angles with few balls in play.'
SEC2019NormalLaunches<-SEC2019%>%
  filter(Angle>=-20,Angle<=40,PlayResult!="Undefined")
plot(wOBAbyNormalLaunch$Launch,wOBAbyNormalLaunch$wOBA,xlab = "Launch",ylab = "wOBAcon")
'This plot shows peak wOBAcon at 12-13 degrees, but elimninates the noise at the extremes where few balls were hit.'
'After looking at plots, I had the idea of breaking launch angles apart at peak wOBAcon of 12-13 degrees for predictions, though not perfect. Machine learning is better.'
SEC2019NormalLowLaunches<-SEC2019%>%
  filter(Angle>=-20,Angle<=13,PlayResult!="Undefined")
SEC2019NormalHighLaunches<-SEC2019%>%
  filter(Angle>=13,Angle<=40,PlayResult!="Undefined")
wOBAbyNormalLaunch<-SEC2019NormalLaunches%>%
  group_by(cut(SEC2019NormalLaunches$Angle,breaks=62))%>%
  summarize(BIP=sum(PlayResult!="Undefined"),Launch=mean(Angle,na.rm=TRUE),wOBAcon=(((.870*(sum(PlayResult=="Single")))+(1.217*(sum(PlayResult=="Double")))+(1.529*(sum(PlayResult=="Triple")))+(1.940*(sum(PlayResult=="HomeRun")))))/(BIP))
wOBAbyNormalLowLaunch<-SEC2019NormalLowLaunches%>%
  group_by(cut(SEC2019NormalLowLaunches$Angle,breaks=34))%>%
  summarize(BIP=sum(PlayResult!="Undefined"),Launch=mean(Angle,na.rm=TRUE),wOBAcon=(((.870*(sum(PlayResult=="Single")))+(1.217*(sum(PlayResult=="Double")))+(1.529*(sum(PlayResult=="Triple")))+(1.940*(sum(PlayResult=="HomeRun")))))/(BIP))
wOBAbyNormalHighLaunch<-SEC2019NormalHighLaunches%>%
  group_by(cut(SEC2019NormalHighLaunches$Angle,breaks=28))%>%
  summarize(BIP=sum(PlayResult!="Undefined"),Launch=mean(Angle,na.rm=TRUE),wOBAcon=(((.870*(sum(PlayResult=="Single")))+(1.217*(sum(PlayResult=="Double")))+(1.529*(sum(PlayResult=="Triple")))+(1.940*(sum(PlayResult=="HomeRun")))))/(BIP))
plot(wOBAbyNormalLowLaunch$Launch,wOBAbyNormalLowLaunch$wOBA)
plot(wOBAbyNormalHighLaunch$Launch,wOBAbyNormalHighLaunch$wOBA)
'I went through the same process with high and low launches as all launches.'
wOBALowLaunchEquation<-lm(wOBA~Launch,data=wOBAbyNormalLowLaunch)
summary(wOBALowLaunchEquation)
wOBAHighLaunchEquation<-lm(wOBA~Launch,data=wOBAbyNormalHighLaunch)
summary(wOBAHighLaunchEquation)
'Formulas work well for predicting their respective grouping of wOBAcon.'
'Find wOBA by pitch type.'
SEC2019wOBAbyPitchType<-SEC2019%>%
  group_by(TaggedPitchType)%>%
  summarize(InPlay=sum(PlayResult!="Undefined"),AvgLaunch=mean(Angle,na.rm=TRUE),wOBAcon=(((.870*(sum(PlayResult=="Single")))+(1.217*(sum(PlayResult=="Double")))+(1.529*(sum(PlayResult=="Triple")))+(1.940*(sum(PlayResult=="HomeRun")))))/(InPlay))
'wOBAcon show curveballs are more effective than sliders. Changeups are effective.'
'Sample size is relatively small, but cutters are effective.'
'Study effectiveness is limited by human error in tagging pitches across SEC.'
'Whiff rates and strike rates must be considered for pitch effectiveness.'
SEC2019PitchTypeRates<-SEC2019%>%
  group_by(TaggedPitchType)%>%
  summarize(PitchCount=sum(TaggedPitchType!=""),NotInPlayCount=sum(PitchCall!="InPlay"),SwingCount=sum(PitchCall=="StrikeSwinging"|PitchCall=="FoulBall"|PitchCall=="InPlay"),WhiffCount=sum(PitchCall=="StrikeSwinging"),StrikeCount=(WhiffCount)+sum(PitchCall=="StrikeCalled"|PitchCall=="FoulBall"))%>%
  mutate(WhiffRate=(WhiffCount)/(SwingCount),StrikeRate=(StrikeCount)/(PitchCount))%>%
  select(TaggedPitchType,NotInPlayCount,WhiffRate,StrikeRate)
'While it comes with small sample size again, cutters carry a far higher whiff rate than fastballs.'
'Cutters also carry the highest strike rate overall.'
'Sliders are preferable to curveballs, both for generating strikes and whiffs.'
'Now I will look into which stat between Avg Launch Angle, Whiff%, and Strike% is most impactful for FIP.'
SEC2019FIPfromRatesAngle<-SEC2019%>%
  group_by(Pitcher)%>%
  summarize(IP=sum(OutsOnPlay)/3,WalkCount=sum(KorBB=="Walk"|PitchCall=="HitByPitch"),HRCount=sum(PlayResult=="HomeRun"),KCount=sum(KorBB=="Strikeout"),AvgLaunch=mean(Angle,na.rm=TRUE),PitchCount=sum(TaggedPitchType!=""),NotInPlayCount=sum(PitchCall!="InPlay"),SwingCount=sum(PitchCall=="StrikeSwinging"|PitchCall=="FoulBall"|PitchCall=="InPlay"),WhiffCount=sum(PitchCall=="StrikeSwinging"),StrikeCount=(WhiffCount)+sum(PitchCall=="StrikeCalled"|PitchCall=="FoulBall"))%>%
  filter(IP>=20)%>%
  mutate(FIP=((13*(HRCount)+3*(WalkCount)-2*(KCount))/(IP))+(SEC2019FIPConstant))%>%
  mutate(WhiffRate=(WhiffCount)/(SwingCount),StrikeRate=(StrikeCount)/(PitchCount))%>%
  select(FIP,AvgLaunch,WhiffRate,StrikeRate)
'Filtered out less than 20 innings to keep data stronger.'
'SEC FIP constant was calculated earlier.'
'Chart FIP on X-axis with changing Y-axis.'
plot(SEC2019FIPfromRatesAngle$FIP,SEC2019FIPfromRatesAngle$AvgLaunch)
plot(SEC2019FIPfromRatesAngle$FIP,SEC2019FIPfromRatesAngle$WhiffRate)
plot(SEC2019FIPfromRatesAngle$FIP,SEC2019FIPfromRatesAngle$StrikeRate)
'Use corrr to investigate correlations.'
SEC2019FIPfromRatesAngle%>%
  correlate()%>%
  focus(FIP)
'Correlation shows strong negative correlation between strike rate and FIP.'
'Medium negative correlation between strike rate and FIP.'
'Very low correlation between Launch Angle and FIP.'
'While more consistent pitch type tagging would be ideal, the data suggests cutters should be used more, as well as sliders. A changeup should round out the repertoire.'

