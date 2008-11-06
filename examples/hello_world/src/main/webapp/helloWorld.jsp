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
                <h:outputLabel for="first_name" value="First name:"/>
                <h:inputText id="first_name" value="#{helloWorld.person.firstName}"/>
                <h:message for="first_name" showSummary="true" showDetail="false"/>

                <h:outputLabel for="last_name" value="Last name:"/>
                <h:inputText id="last_name" value="#{helloWorld.person.lastName}"/>
                <h:message for="last_name" showSummary="true" showDetail="false"/>

                <h:commandButton value="Press me" action="#{helloWorld.send}"/>
                <h:panelGroup/>
                <h:panelGroup/>
              </h:panelGrid>
            </h:form>
        </f:view>
    </body>
</html>
