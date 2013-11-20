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
package org.apache.myfaces.extensions.validator.core.factory;

import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;

import javax.el.ValueExpression;
import javax.faces.FacesException;
import javax.faces.application.ApplicationWrapper;
import javax.faces.application.Application;
import javax.faces.application.Resource;
import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

/**
 * @since r4
 */
class ExtValApplicationWrapper extends ApplicationWrapper
{
    private static final boolean DEACTIVATE_VIEW_ROOT_INTERCEPTOR =
            ExtValCoreConfiguration.get().deactivateViewRootInterceptor();

    private Application wrapped;

    ExtValApplicationWrapper(Application wrapped)
    {
        this.wrapped = wrapped;
    }

    public Application getWrapped()
    {
        return this.wrapped;
    }

    @Override
    public void addDefaultValidatorId(String s)
    {
        if(!"javax.faces.Bean".equals(s))
        {
            super.addDefaultValidatorId(s);
        }
    }

    @Override
    public UIComponent createComponent(FacesContext context, Resource componentResource)
    {
        UIComponent result = super.createComponent(context, componentResource);
        return customizedComponent(result);
    }

    @Override
    public UIComponent createComponent(FacesContext context, String componentType, String rendererType)
    {
        UIComponent result = super.createComponent(context, componentType, rendererType);
        return customizedComponent(result);
    }

    @Override
    public UIComponent createComponent(String componentType) throws FacesException
    {
        UIComponent result = super.createComponent(componentType);
        return customizedComponent(result);
    }

    @Override
    public UIComponent createComponent(ValueBinding componentBinding, FacesContext context, String componentType)
            throws FacesException
    {
        UIComponent result = super.createComponent(componentBinding, context, componentType);
        return customizedComponent(result);
    }

    @Override
    public UIComponent createComponent(
            ValueExpression componentExpression, FacesContext context, String componentType, String rendererType)
    {
        UIComponent result = super.createComponent(componentExpression, context, componentType, rendererType);
        return customizedComponent(result);
    }

    @Override
    public UIComponent createComponent(ValueExpression componentExpression, FacesContext contexte, String componentType)
            throws FacesException
    {
        UIComponent result = super.createComponent(componentExpression, contexte, componentType);
        return customizedComponent(result);
    }

    private UIComponent customizedComponent(UIComponent result)
    {
        if (DEACTIVATE_VIEW_ROOT_INTERCEPTOR)
        {
            return result;
        }

        //don't check with instanceof
        //if it isn't javax.faces.component.UIViewRoot itself, we need to proxy it
        //due to the overhead we should wait for users who request such a proxy
        if(result != null && result.getClass().getName().equals(UIViewRoot.class.getName()))
        {
            return new ExtValViewRoot();
        }
        return result;
    }
}
