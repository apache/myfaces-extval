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
package org.apache.myfaces.extensions.validator.test.base.mock;

import org.apache.myfaces.extensions.validator.core.el.ELHelper;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;

import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;

public class MockELHelper implements ELHelper
{
    private ELHelper wrapped;

    public MockELHelper(ELHelper wrapped)
    {
        this.wrapped = wrapped;
    }

    public Object getBean(String beanName)
    {
        return wrapped.getBean(beanName);
    }

    public Object getValueOfExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression)
    {
        if("#{true}".equals(valueBindingExpression.getExpressionString()))
        {
            return Boolean.TRUE;
        }

        return wrapped.getValueOfExpression(facesContext, valueBindingExpression);
    }

    public Class getTypeOfExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression)
    {
        return wrapped.getTypeOfExpression(facesContext, valueBindingExpression);
    }

    public PropertyDetails getPropertyDetailsOfValueBinding(UIComponent uiComponent)
    {
        return wrapped.getPropertyDetailsOfValueBinding(uiComponent);
    }

    public boolean isELTermValid(FacesContext facesContext, String valueBindingExpression)
    {
        return wrapped.isELTermValid(facesContext, valueBindingExpression);
    }

    public boolean isELTermWellFormed(Object o)
    {
        return wrapped.isELTermWellFormed(o);
    }

    public Object getBindingOfComponent(UIComponent uiComponent, String name)
    {
        return wrapped.getBindingOfComponent(uiComponent, name);
    }
}
