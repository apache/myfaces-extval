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
package org.apache.myfaces.extensions.validator.core.el;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/**
 * Encapulates EL handling to isolate the dependency to the specific jsf version.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface ELHelper
{
    /**
     * Resolves the bean instance for the specified name.
     *
     * @param beanName The bean name we are interested in, no EL expression.
     * @return instance for the bean name or null.
     */
    Object getBean(String beanName);

    /**
     * Evaluates the given expression within the current {@link javax.faces.context.FacesContext}
     *
     * @param facesContext <code>FacesContext</code> for the current request
     * @param valueBindingExpression value-binding to evaluate
     * @return result for the given expression
     */
    Object getValueOfExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression);

    /**
     * Evaluates the type returned by {@link #getValueOfExpression}
     *
     * @param facesContext <code>FacesContext</code> for the current request
     * @param valueBindingExpression value-binding to evaluate
     * @return type of the result of the expression
     */
    Class getTypeOfExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression);

    /**
     * Extracts the property details (expression, base object and property name) information of the value attribute of
     * the given {@link javax.faces.component.UIComponent}
     * value attribute.
     *
     * @param uiComponent The {@link javax.faces.component.UIComponent} in question
     * @return property details for the value of the given {@link javax.faces.component.UIComponent}
     */
    PropertyDetails getPropertyDetailsOfValueBinding(UIComponent uiComponent);

    /**
     * Determines if the expression is a valid expression, meaning that the expression can be evaluated without
     * an exception.
     *
     * @param facesContext The JSF Context
     * @param valueBindingExpression The EL expression we want to evaluate.
     * @return true if the expression is valid - false otherwise
     */
    boolean isELTermValid(FacesContext facesContext, String valueBindingExpression);

    /**
     * A very basic check of the EL expression
     *
     * @param o Object in question
     * @return true if the given object is a well formed string - false otherwise
     */
    boolean isELTermWellFormed(Object o);

    /**
     * Gets the value of the attribute specified  by the parameter name for the UIComponent.
     *
     * @param uiComponent The {@link javax.faces.component.UIComponent} in question
     * @param name The name of the attribute.
     * @return The ValueExpression assigned to the attribute.
     */
    @Deprecated
    Object getBindingOfComponent(UIComponent uiComponent, String name);
}
