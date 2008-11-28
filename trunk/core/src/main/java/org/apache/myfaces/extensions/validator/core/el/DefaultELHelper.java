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
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.core.WebXmlParameter;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.myfaces.extensions.validator.ExtValInformation;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import javax.el.ValueExpression;
import javax.el.ELContext;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import java.io.Externalizable;

/**
 * in order to centralize the jsf version dependency within the core
 *
 * this el-helper supports jsp and facelets (tested with 1.1.14)
 *
 * @author Gerhard Petracek
 * @since 1.x.1
 */
/*
 * there is a special facelets workaround for el-expressions of custom components
 * it's pluggable in order to support special mechanisms of different technologies (than jsp and facelets)
 * so you can plug in your own impl. which implements a custom workaround (like the facelets workaround of this impl.)
 */
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultELHelper implements ELHelper
{
    private static final String DEACTIVATE_EL_RESOLVER = WebXmlParameter.DEACTIVATE_EL_RESOLVER;

    protected final Log logger = LogFactory.getLog(getClass());

    public DefaultELHelper()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public Class getTypeOfExpression(FacesContext facesContext, ValueBindingExpression valueBindingExpression)
    {
        //due to a restriction with the ri
        Object bean = getValueOfExpression(facesContext, valueBindingExpression);
        return (bean != null) ? bean.getClass() : null;
    }

    public Object getBean(String beanName)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        return facesContext.getApplication().getELResolver().getValue(facesContext.getELContext(), null, beanName);
    }

    public Object getValueOfExpression(FacesContext facesContext,
                                                   ValueBindingExpression valueBindingExpression)
    {
        return (valueBindingExpression != null) ? facesContext.getApplication().evaluateExpressionGet(
            facesContext, valueBindingExpression.getExpressionString(), Object.class) : null;
    }

    public boolean isExpressionValid(FacesContext facesContext, String valueBindingExpression)
    {
        try
        {
            facesContext.getApplication().evaluateExpressionGet(facesContext, valueBindingExpression, Object.class);
        }
        catch (Throwable t)
        {
            return false;
        }
        return true;
    }

    private ValueBindingExpression getValueBindingExpression(UIComponent uiComponent, boolean allowBlankCharacters)
    {
        String valueBindingExpression = getOriginalValueBindingExpression(uiComponent);

        //for input components without value-binding
        //(e.g. for special component libs -> issue with ExtValRendererWrapper#encodeBegin)
        if(valueBindingExpression == null)
        {
            if(this.logger.isTraceEnabled())
            {
                this.logger.trace(
                        uiComponent.getClass() + " has no value binding - component id: " + uiComponent.getId());
            }
            return null;
        }

        if(!allowBlankCharacters)
        {
            valueBindingExpression = valueBindingExpression.replace(" ", "");
        }

        if (getTypeOfExpression(FacesContext.getCurrentInstance(),
            new ValueBindingExpression(valueBindingExpression).getBaseExpression()) == null)
        {
            ValueBindingExpression result = FaceletsTaglibExpressionHelper.
                tryToCreateValueBindingForFaceletsBinding(uiComponent);

            if(result == null)
            {
                if(logger.isWarnEnabled())
                {
                    logger.warn("couldn't resolve expression: " + valueBindingExpression);
                }
                return null;
            }

            Class entityClass = ExtValUtils.getELHelper()
                .getTypeOfExpression(FacesContext.getCurrentInstance(), result.getBaseExpression());

            if(entityClass == null)
            {
                if(logger.isWarnEnabled())
                {
                    logger.warn("couldn't resolve expression: " + result.getExpressionString());
                }

                return null;
            }
            return result;
        }
        return new ValueBindingExpression(valueBindingExpression);
    }

    public PropertyDetails getPropertyDetailsOfValueBinding(UIComponent uiComponent)
    {
        if("true".equalsIgnoreCase(DEACTIVATE_EL_RESOLVER))
        {
            return getPropertyDetailsViaReflectionFallback(uiComponent);
        }

        FacesContext facesContext = FacesContext.getCurrentInstance();

        ExtValELResolver elResolver = new ExtValELResolver(facesContext.getApplication().getELResolver());
        ELContext elContext = ExtValELResolver.createContextWrapper(facesContext.getELContext(), elResolver);

        ValueExpression valueExpression = uiComponent.getValueExpression("value");

        if(valueExpression == null)
        {
            return null;
        }

        try
        {
            valueExpression.setValue(elContext, null);
        }
        catch (Throwable t)
        {
            throw new IllegalStateException(
                "error at binding: " + valueExpression.getExpressionString() +
                " -- an el-resolver error occurred! maybe you used an invalid binding. otherwise: " +
                "please report the issue, deactivate the el-resovler of extval via web.xml context-param: " +
                ExtValInformation.WEBXML_PARAM_PREFIX + ".DEACTIVATE_EL_RESOLVER" +
                " and test again.");
        }

        if(elResolver.getPath() == null || elResolver.getBaseObject() == null || elResolver.getProperty() == null)
        {
            return null;
        }

        return new PropertyDetails(elResolver.getPath(), elResolver.getBaseObject(), elResolver.getProperty());
    }

    private PropertyDetails getPropertyDetailsViaReflectionFallback(UIComponent uiComponent)
    {
        ValueBindingExpression valueBindingExpression = getValueBindingExpression(uiComponent, false);

        if(valueBindingExpression == null)
        {
            return null;
        }

        ValueBindingExpression currentValueBindingExpression =
            new ValueBindingExpression(valueBindingExpression.getExpressionString());
        String path = null;

        while(currentValueBindingExpression.getBaseExpression() != null)
        {
            if(path == null)
            {
                path = currentValueBindingExpression.getProperty();
            }
            else
            {
                path = currentValueBindingExpression.getProperty() + "." + path;
            }

            currentValueBindingExpression = currentValueBindingExpression.getBaseExpression();
        }

        path = currentValueBindingExpression.getProperty() + "." + path;

        Object baseObject = getValueOfExpression(
                FacesContext.getCurrentInstance(), valueBindingExpression.getBaseExpression());
        return new PropertyDetails(path, baseObject, valueBindingExpression.getProperty());
    }

    static String getOriginalValueBindingExpression(UIComponent uiComponent)
    {
        ValueExpression valueExpression = uiComponent.getValueExpression("value");

        return (valueExpression != null) ? valueExpression.getExpressionString() : null;
    }

    public boolean isELTerm(Object o)
    {
        if (o instanceof ValueBinding || o instanceof Externalizable)
        {
            return false;
        }

        String s = o.toString();
        return ((s.contains("#") || s.contains("$")) && s.contains("{") && s.contains("}"));
    }

    public Object getBindingOfComponent(UIComponent uiComponent, String name)
    {
        return uiComponent.getValueExpression(name);
    }
}
