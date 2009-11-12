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
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultViolationSeverityInterpreter implements ViolationSeverityInterpreter
{
    public boolean severityBlocksNavigation(
            FacesContext facesContext, UIComponent uiComponent, FacesMessage.Severity severity)
    {
        return FacesMessage.SEVERITY_ERROR.equals(severity) || FacesMessage.SEVERITY_FATAL.equals(severity);
    }

    public boolean severityCausesValidatorException(
            FacesContext facesContext, UIComponent uiComponent, FacesMessage.Severity severity)
    {
        return FacesMessage.SEVERITY_ERROR.equals(severity) || FacesMessage.SEVERITY_FATAL.equals(severity);
    }

    public boolean severityCausesViolationMessage(
            FacesContext facesContext, UIComponent uiComponent, FacesMessage.Severity severity)
    {
        return true;
    }

    public boolean severityBlocksSubmit(
            FacesContext facesContext, UIComponent uiComponent, FacesMessage.Severity severity)
    {
        return FacesMessage.SEVERITY_ERROR.equals(severity) || FacesMessage.SEVERITY_FATAL.equals(severity);
    }

    public boolean severityShowsIndication(
            FacesContext facesContext, UIComponent uiComponent, FacesMessage.Severity severity)
    {
        return FacesMessage.SEVERITY_ERROR.equals(severity) || FacesMessage.SEVERITY_FATAL.equals(severity);
    }
}