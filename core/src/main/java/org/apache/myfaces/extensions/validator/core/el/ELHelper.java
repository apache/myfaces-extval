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

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

/**
 * in order to centralize the jsf version dependency within the core
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@UsageInformation(UsageCategory.API)
public interface ELHelper
{
    Class getTypeOfValueBindingForExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression);

    Class getTypeOfValueBindingForComponent(FacesContext facesContext, UIComponent uiComponent);

    Object getBean(String beanName);

    Object getBaseObject(ValueBindingExpression valueBindingExpression);

    Object getBaseObject(ValueBindingExpression valueBindingExpression, UIComponent uiComponent);

    Object getValueOfExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression);

    ValueBindingExpression getValueBindingExpression(UIComponent uiComponent);

    boolean isExpressionValid(FacesContext facesContext, String valueBindingExpression);

    boolean isELTerm(Object o);

    Object getBindingOfComponent(UIComponent uiComponent, String name);
}