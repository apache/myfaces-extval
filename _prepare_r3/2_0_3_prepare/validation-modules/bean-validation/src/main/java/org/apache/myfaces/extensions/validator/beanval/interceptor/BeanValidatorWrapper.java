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
package org.apache.myfaces.extensions.validator.beanval.interceptor;

import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;

import javax.faces.validator.BeanValidator;
import javax.faces.context.FacesContext;
import javax.faces.component.UIComponent;

/**
 * replacement for BeanValidator which gets added due to f:validateBean
 *
 * @author Gerhard Petracek
 * @since 2.x.3
 */
@UsageInformation(UsageCategory.INTERNAL)
class BeanValidatorWrapper extends BeanValidator
{
    private BeanValidator wrapped;

    BeanValidatorWrapper(BeanValidator wrapped)
    {
        this.wrapped = wrapped;
    }

    public BeanValidator getWrappedBeanValidator()
    {
        return this.wrapped;
    }

    public void setWrapped(BeanValidator wrapped)
    {
        this.wrapped = wrapped;
    }

    public void setValidationGroups(String s)
    {
        wrapped.setValidationGroups(s);
    }

    public String getValidationGroups()
    {
        return wrapped.getValidationGroups();
    }

    public void validate(FacesContext facesContext, UIComponent uiComponent, Object o)
    {
        //don't validate - the extval bean-validation adapter will do that
    }

    /*
    public Object saveState(FacesContext facesContext)
    {
        Object result[] = new Object[1];
        result[0] = wrapped.getValidationGroups();
        return result;
    }

    public void restoreState(FacesContext facesContext, Object state)
    {
        this.wrapped = new BeanValidator();

        if (state != null)
        {
            Object values[] = (Object[]) state;
            this.wrapped.setValidationGroups((String) values[0]);
        }
    }
    */

    public void markInitialState()
    {
        wrapped.markInitialState();
    }

    public boolean initialStateMarked()
    {
        return wrapped.initialStateMarked();
    }

    public void clearInitialState()
    {
        wrapped.clearInitialState();
    }

    public boolean isTransient()
    {
        return wrapped.isTransient();
    }

    public void setTransient(boolean b)
    {
        wrapped.setTransient(b);
    }
}
