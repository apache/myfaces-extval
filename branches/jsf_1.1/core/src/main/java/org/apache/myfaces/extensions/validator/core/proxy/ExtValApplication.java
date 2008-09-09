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
package org.apache.myfaces.extensions.validator.core.proxy;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.myfaces.extensions.validator.internal.UsageEnum;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.util.FactoryUtils;

import javax.faces.FacesException;
import javax.faces.application.Application;
import javax.faces.application.NavigationHandler;
import javax.faces.application.StateManager;
import javax.faces.application.ViewHandler;
import javax.faces.component.EditableValueHolder;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.convert.Converter;
import javax.faces.el.MethodBinding;
import javax.faces.el.PropertyResolver;
import javax.faces.el.ReferenceSyntaxException;
import javax.faces.el.ValueBinding;
import javax.faces.el.VariableResolver;
import javax.faces.event.ActionListener;
import javax.faces.validator.Validator;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;

/**
 * @author Gerhard Petracek
 */
@UsageInformation({UsageEnum.ALTERNATIVE, UsageEnum.INTERNAL})
public class ExtValApplication extends Application
{
    protected final Log logger = LogFactory.getLog(getClass());

    private Application wrapped;

    public ExtValApplication()
    {
    }

    public ExtValApplication(Application wrapped)
    {
        if (logger.isTraceEnabled())
        {
            logger.trace(getClass().getName() + " wraps " + wrapped.getClass().getName());
        }
        this.wrapped = wrapped;
    }

    public UIComponent createComponent(ValueBinding componentBinding,
                                       FacesContext context, String componentType) throws FacesException
    {
        UIComponent component = this.wrapped.createComponent(componentBinding,
            context, componentType);
        return tryToSetExtValValidatingConverter(component);
    }

    private UIComponent tryToSetExtValValidatingConverter(UIComponent component)
    {
        //in order to access the wrapped application and and support other Application wrappers
        ExtValUtils.setOriginalApplication(wrapped);

        //if no converter is used add sev-en converter - so it isn't 
        //necessary to add sev-en converter manually within the page
        if (component instanceof EditableValueHolder)
        {
            ((EditableValueHolder) component)
                .setConverter(new ExtValConverter());
        }
        return component;
    }

    public Converter createConverter(String converterId) throws FacesException
    {
        Converter converter = this.wrapped.createConverter(converterId);
        return getExtValConverter(converter);
    }

    public Converter createConverter(Class targetClass) throws FacesException
    {
        Converter converter = this.wrapped.createConverter(targetClass);

        return getExtValConverter(converter);
    }

    private Converter getExtValConverter(Converter converter)
    {
        if (converter == null)
        {
            return new ExtValConverter();
        }

        if (this.logger.isTraceEnabled())
        {
            this.logger.trace("converter to wrap: " + converter.getClass().getName());
        }

        if (!ExtValUtils.useFallbackAdapters())
        {
            return ExtValConverter.newInstance(converter);
        }
        else
        {
            //fallback adapter solution
            //if there is a problem with the default approach (the phase-listener)
            //or the alternative (the state-manager)
            return FactoryUtils.getConverterAdapterFactory().create(converter);
        }
    }

    public Iterator getConverterIds()
    {
        return this.wrapped.getConverterIds();
    }

    public Iterator getConverterTypes()
    {
        return this.wrapped.getConverterTypes();
    }

    public void addConverter(String converterId, String converterClass)
    {
        this.wrapped.addConverter(converterId, converterClass);
    }

    public void addConverter(Class targetClass, String converterClass)
    {
        this.wrapped.addConverter(targetClass, converterClass);
    }

    public Iterator getComponentTypes()
    {
        return this.wrapped.getComponentTypes();
    }

    public ActionListener getActionListener()
    {
        return this.wrapped.getActionListener();
    }

    public void setActionListener(ActionListener listener)
    {
        this.wrapped.setActionListener(listener);
    }

    public Locale getDefaultLocale()
    {
        return this.wrapped.getDefaultLocale();
    }

    public void setDefaultLocale(Locale locale)
    {
        this.wrapped.setDefaultLocale(locale);
    }

    public String getDefaultRenderKitId()
    {
        return this.wrapped.getDefaultRenderKitId();
    }

    public void setDefaultRenderKitId(String renderKitId)
    {
        this.wrapped.setDefaultRenderKitId(renderKitId);
    }

    public String getMessageBundle()
    {
        return this.wrapped.getMessageBundle();
    }

    public void setMessageBundle(String bundle)
    {
        this.wrapped.setMessageBundle(bundle);
    }

    public NavigationHandler getNavigationHandler()
    {
        return this.wrapped.getNavigationHandler();
    }

    public void setNavigationHandler(NavigationHandler handler)
    {
        this.wrapped.setNavigationHandler(handler);
    }

    public PropertyResolver getPropertyResolver()
    {
        return this.wrapped.getPropertyResolver();
    }

    public void setPropertyResolver(PropertyResolver resolver)
    {
        this.wrapped.setPropertyResolver(resolver);
    }

    public VariableResolver getVariableResolver()
    {
        return this.wrapped.getVariableResolver();
    }

    public void setVariableResolver(VariableResolver resolver)
    {
        this.wrapped.setVariableResolver(resolver);
    }

    public ViewHandler getViewHandler()
    {
        return this.wrapped.getViewHandler();
    }

    public void setViewHandler(ViewHandler handler)
    {
        this.wrapped.setViewHandler(handler);
    }

    public StateManager getStateManager()
    {
        return this.wrapped.getStateManager();
    }

    public void setStateManager(StateManager manager)
    {
        this.wrapped.setStateManager(manager);
    }

    public void addComponent(String componentType, String componentClass)
    {
        this.wrapped.addComponent(componentType, componentClass);
    }

    public UIComponent createComponent(String componentType)
        throws FacesException
    {
        UIComponent component = this.wrapped.createComponent(componentType);

        return tryToSetExtValValidatingConverter(component);
    }

    public MethodBinding createMethodBinding(String ref, Class[] params)
        throws ReferenceSyntaxException
    {
        return this.wrapped.createMethodBinding(ref, params);
    }

    public Iterator getSupportedLocales()
    {
        return this.wrapped.getSupportedLocales();
    }

    public void setSupportedLocales(Collection locales)
    {
        this.wrapped.setSupportedLocales(locales);
    }

    public void addValidator(String validatorId, String validatorClass)
    {
        this.wrapped.addValidator(validatorId, validatorClass);
    }

    public Validator createValidator(String validatorId) throws FacesException
    {
        return this.wrapped.createValidator(validatorId);
    }

    public Iterator getValidatorIds()
    {
        return this.wrapped.getValidatorIds();
    }

    public ValueBinding createValueBinding(String ref)
        throws ReferenceSyntaxException
    {
        return this.wrapped.createValueBinding(ref);
    }
}
