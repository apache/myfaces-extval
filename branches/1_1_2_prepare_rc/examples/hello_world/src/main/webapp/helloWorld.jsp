<%--
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
--%>

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
                <h:message for="first_name" showSummary="false" showDetail="true"/>

                <h:outputLabel for="last_name" value="Last name:"/>
                <h:inputText id="last_name" value="#{helloWorld.person.lastName}"/>
                <h:message for="last_name" showSummary="false" showDetail="true"/>

                <h:commandButton value="Press me" action="#{helloWorld.send}"/>
                <h:panelGroup/>
                <h:panelGroup/>
              </h:panelGrid>
            </h:form>
        </f:view>
    </body>
</html>
