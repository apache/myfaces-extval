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
 * Structure of the object that handles EL here centralized to isolate the jsf version dependency within the core.
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface ELHelper
{
    /**
     * Return the bean instance from the JSF contexts with the specified name.
     * @param beanName The bean name we are interested in, no EL expression.
     * @return instance for the bean name or null.
     */
    Object getBean(String beanName);

    /**
     * Evaluates the expression equivalent of the ValueBindingExpression parameter and returns the result.
     * @param facesContext The JSF Context
     * @param valueBindingExpression information about expression to evaluate.
     * @return result of the expression equivalent of the ValueBindingExpression parameter
     */
    Object getValueOfExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression);

    /**
     * Returns the type of the expression equivalent of the ValueBindingExpression parameter.
     * @param facesContext The JSF Context
     * @param valueBindingExpression information about expression to evaluate.
     * @return type of the expression equivalent of the ValueBindingExpression parameter
     */
    Class getTypeOfExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression);

    /**
     * Extracts the property details (expression, base object and property name) information from the UIComponent's
     * value attribute.
     * @param uiComponent The UIComponent of interest.
     * @return property details information
     */
    PropertyDetails getPropertyDetailsOfValueBinding(UIComponent uiComponent);

    /**
     * Determines if the expression is a valid expression, meaning that the expression can be evaluated without
     * exception.
     * @param facesContext The JSF Context
     * @param valueBindingExpression The EL expression we want to evaluate.
     * @return valid EL expression ?
     */
    boolean isELTermValid(FacesContext facesContext, String valueBindingExpression);

    /**
     * A very basic check of the EL expression is well formed.
     * @param o Object
     * @return
     */
    boolean isELTermWellFormed(Object o);

    /**
     * Gets the value of the attribute specified  by the parameter name for the UIComponent.
     *
     * @param uiComponent The UIComponent
     * @param name The name of the attribute.
     * @return The ValueExpression assigned to the attribute.
     */
    Object getBindingOfComponent(UIComponent uiComponent, String name);
}
