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

import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.core.property.PropertyDetails;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

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
@ToDo(Priority.MEDIUM)
@UsageInformation(UsageCategory.INTERNAL)
public class DefaultELHelper implements ELHelper
{
    protected final Log logger = LogFactory.getLog(getClass());

    public DefaultELHelper()
    {
        if(logger.isDebugEnabled())
        {
            logger.debug(getClass().getName() + " instantiated");
        }
    }

    public Class getTypeOfValueBindingForExpression(FacesContext facesContext,
                                                    ValueBindingExpression valueBindingExpression)
    {
        //due to a restriction with the ri
        Object bean = getValueOfExpression(facesContext, valueBindingExpression);
        return (bean != null) ? bean.getClass() : null;
    }

    public Object getBean(String beanName)
    {
        FacesContext facesContext = FacesContext.getCurrentInstance();
        return facesContext.getApplication().getVariableResolver().resolveVariable(facesContext, beanName);
    }

    @ToDo(value = Priority.MEDIUM, description = "refactor - problem - static values - jsf 1.2 e.g.: ${value}")
    public Object getBaseObject(ValueBindingExpression valueBindingExpression, UIComponent uiComponent)
    {
        if(valueBindingExpression.getBaseExpression() == null)
        {
            return uiComponent.getValueBinding("value").getValue(FacesContext.getCurrentInstance());
        }
        return getBaseObject(valueBindingExpression);
    }

    public Object getBaseObject(ValueBindingExpression valueBindingExpression)
    {
        return getValueOfExpression(FacesContext.getCurrentInstance(),
            valueBindingExpression.getBaseExpression());
    }

    public Object getValueOfExpression(FacesContext facesContext,
                                                   ValueBindingExpression valueBindingExpression)
    {
        return (valueBindingExpression != null) ? facesContext.getApplication()
            .createValueBinding(valueBindingExpression.getExpressionString()).getValue(facesContext) : null;
    }

    public boolean isExpressionValid(FacesContext facesContext, String valueBindingExpression)
    {
        return facesContext.getApplication().createValueBinding(valueBindingExpression) != null;
    }

    private ValueBindingExpression getValueBindingExpression(UIComponent uiComponent)
    {
        return getValueBindingExpression(uiComponent, false);
    }

    private ValueBindingExpression getValueBindingExpression(UIComponent uiComponent, boolean allowBlankCharacters)
    {
        String valueBindingExpression = getOriginalValueBindingExpression(uiComponent);

        //for input components without value-binding
        //(e.g. for special component libs -> issue with ExtValRendererWrapper#encodeBegin)
        if(valueBindingExpression == null)
        {
            //TODO logging
            return null;
        }

        if(!allowBlankCharacters)
        {
            valueBindingExpression = valueBindingExpression.replace(" ", "");
        }

        if (getTypeOfValueBindingForExpression(FacesContext.getCurrentInstance(),
            new ValueBindingExpression(valueBindingExpression).getBaseExpression()) == null)
        {
            ValueBindingExpression result = FaceletsTaglibExpressionHelper.
                tryToCreateValueBindingForFaceletsBinding(uiComponent);

            if(result == null)
            {
                if(logger.isWarnEnabled())
                {
                    logger.warn("couldn't resolve expression: " + result.getExpressionString());
                }
                return null;
            }

            Class entityClass = ExtValUtils.getELHelper()
                .getTypeOfValueBindingForExpression(FacesContext.getCurrentInstance(), result.getBaseExpression());

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

    public PropertyDetails getTargetInformation(UIComponent uiComponent)
    {
        ValueBindingExpression valueBindingExpression = getValueBindingExpression(uiComponent, false);
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

        return new PropertyDetails(path,
            getBaseObject(valueBindingExpression), valueBindingExpression.getProperty());
    }

    static String getOriginalValueBindingExpression(UIComponent uiComponent)
    {
        ValueBinding valueExpression = uiComponent.getValueBinding("value");

        return (valueExpression != null) ? valueExpression.getExpressionString() : null;
    }

    public Class getTypeOfValueBindingForComponent(FacesContext facesContext, UIComponent uiComponent)
    {
        ValueBinding valueBinding = uiComponent.getValueBinding("value");

        return (valueBinding != null) ? valueBinding.getType(facesContext) : null;
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
        return uiComponent.getValueBinding(name);
    }
}
