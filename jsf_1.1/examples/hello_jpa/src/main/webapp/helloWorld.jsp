<%@ taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<%@ taglib uri="http://java.sun.com/jsf/core" prefix="f"%>
<html>
    <head>
        <title>Hello World</title>
    </head>
    <body>
        <f:view>
            <h:form id="mainForm">
              <h:panelGrid columns="3">
                <h:outputLabel for="name" value="Please enter your name" />
                <h:inputText id="name" value="#{helloWorld.person.name}"/>
                <h:message for="name" showSummary="false" showDetail="true"/>

                <h:commandButton value="Press me" action="#{helloWorld.send}"/>
                <h:panelGroup/>
                <h:panelGroup/>
              </h:panelGrid>
            </h:form>
        </f:view>
    </body>
</html>
