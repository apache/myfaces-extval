/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.core.validation.parameter;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;
import javax.faces.application.FacesMessage;

/**
 * mechanism to change the default behavior of extval
 * 
 * @author Gerhard Petracek
 * @since x.x.3
 */
@UsageInformation(UsageCategory.API)
public interface ViolationSeverityInterpreter
{
    /**
     * @param facesContext current faces context
     * @param uiComponent current component
     * @param severity jsf severity for faces messages
     * @return true if the given severity should block the navigation
     * if #severityCausesValidatorException returns falls validation will be continued for the current property
     * all messages which don't lead to an exception should be stored in a storage and
     * added after the first message which gets thrown as exception
     * a global PropertyValidationInterceptor add the messages of the storage as faces message
     */
    boolean severityBlocksNavigation(
            FacesContext facesContext, UIComponent uiComponent, FacesMessage.Severity severity);

    /**
     * @param facesContext current faces context
     * @param uiComponent current component
     * @param severity jsf severity for faces messages
     * @return true if the given severity should cause a validator exception
     */
    boolean severityCausesValidatorException(
            FacesContext facesContext, UIComponent uiComponent, FacesMessage.Severity severity);

    /**
     *
     * @param facesContext current faces context
     * @param uiComponent current component
     * @param severity jsf severity for faces messages
     * @return true if a violation message leads to a faces message
     */
    boolean severityCausesViolationMessage(
            FacesContext facesContext, UIComponent uiComponent, FacesMessage.Severity severity);

    /**
     * @param facesContext current faces context
     * @param uiComponent current component
     * @param severity jsf severity for faces messages
     * @return true if the constraint with the given severity should be validated on the client side (if supported)
     */
    boolean severityBlocksSubmit(
            FacesContext facesContext, UIComponent uiComponent, FacesMessage.Severity severity);

    /**
     * @param facesContext current faces context
     * @param uiComponent current component
     * @param severity jsf severity for faces messages
     * @return true if the constraint with the given severity
     * should cause e.g. a required marker independent of client-side validation (if supported)
     */
    boolean severityShowsIndication(
            FacesContext facesContext, UIComponent uiComponent, FacesMessage.Severity severity);
}
