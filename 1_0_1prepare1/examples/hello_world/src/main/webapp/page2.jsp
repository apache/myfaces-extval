<%@ taglib uri="http://java.sun.com/jsf/html" prefix="h" %>
<%@ taglib uri="http://java.sun.com/jsf/core" prefix="f"%>
<html>
    <head>
        <title>Hello World</title>
    </head>
    <body>
        <f:view>
            <h:form id="mainForm">
                <h2><h:outputText value="Hello #{helloWorld.person.firstName} #{helloWorld.person.lastName}. We hope you enjoy Apache MyFaces Extensions Validator!"/></h2>
                <h:commandLink action="back">
                    <h:outputText value="Home"/>
                </h:commandLink>
            </h:form>
        </f:view>
    </body>
</html>