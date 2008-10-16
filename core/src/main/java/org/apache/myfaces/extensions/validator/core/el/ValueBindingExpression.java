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

import org.apache.myfaces.extensions.validator.internal.UsageCategory;
import org.apache.myfaces.extensions.validator.internal.UsageInformation;
import org.apache.myfaces.extensions.validator.internal.ToDo;
import org.apache.myfaces.extensions.validator.internal.Priority;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

/**
 * @author Gerhard Petracek
 * @since 1.x.1
 */
@ToDo(value = Priority.MEDIUM, description = "difference between [ and [' - test with more constelations")
@UsageInformation({UsageCategory.API})
public class ValueBindingExpression
{
    private ValueBindingExpression base;
    private String value;
    private String prefix;
    private String token;

    public static ValueBindingExpression replaceOrAddProperty(ValueBindingExpression valueBindingExpression,
                                                              String newProperty)
    {
        if(valueBindingExpression.getBaseExpression() != null)
        {
            return replaceProperty(valueBindingExpression, newProperty);
        }
        else
        {
            return addProperty(valueBindingExpression, newProperty);
        }
    }

    public static ValueBindingExpression replaceProperty(ValueBindingExpression valueBindingExpression,
                                                         String newProperty)
    {
        if(valueBindingExpression.getProperty().endsWith("']"))
        {
            valueBindingExpression = valueBindingExpression.getBaseExpression();
        }

        if(valueBindingExpression.getBaseExpression() != null)
        {
            return addProperty(valueBindingExpression.getBaseExpression(), newProperty);
        }
        else
        {
            return addProperty(valueBindingExpression, newProperty);
        }
    }

    public static ValueBindingExpression addProperty(ValueBindingExpression valueBindingExpression, String newProperty)
    {
        String sourceExpression = valueBindingExpression.getExpressionString();
        String result = sourceExpression.substring(0, sourceExpression.length() - 1);

        if(newProperty.startsWith("['"))
        {
            return new ValueBindingExpression(result + newProperty + "}");
        }
        else
        {
            return new ValueBindingExpression(result + "." + newProperty + "}");
        }
    }

    public ValueBindingExpression(String expression)
    {
        if(!ExtValUtils.getELHelper().isELTerm(expression))
        {
            throw new IllegalStateException(expression + " is no valid el-expression");
        }

        int index1 = expression.lastIndexOf("']");
        int index2 = expression.lastIndexOf(".");

        if(index1 > index2)
        {
            expression = expression.substring(0, index1);

            int index3 = findIndexOfStartingBracket(expression);
            this.value = expression.substring(index3 + 2, index1);
            this.base = new ValueBindingExpression(expression.substring(0, index3) + "}");
            this.token = "['";
        }
        else if( index2 > index1)
        {
            this.value = expression.substring(index2 + 1, expression.length() - 1 );
            this.base = new ValueBindingExpression(expression.substring(0, index2) + "}");
            this.token = ".";
        }
        else
        {
            this.value = expression.substring(2, expression.length() - 1);
            this.prefix = expression.substring(0, 1);
        }
    }

    public String getProperty()
    {
        this.value = this.value.trim();
        
        if("[".equals(this.token))
        {
            if(this.value.startsWith("'"))
            {
                return this.value.substring(1, this.value.length() - 1);
            }
            return this.base.value + this.token + this.value.substring(0, this.value.length()) + "']";
        }
        return value;
    }

    public ValueBindingExpression getBaseExpression()
    {
        return base;
    }

    public String getExpressionString()
    {
        if(this.base != null)
        {
            String baseExpression = this.base.getExpressionString();

            if("['".equals(this.token))
            {
                return baseExpression.substring(0, baseExpression.length() - 1) + this.token + this.value + "']}";
            }
            return baseExpression.substring(0, baseExpression.length() - 1) + this.token + this.value + "}";
        }
        else
        {
            return this.prefix + "{" + this.value + "}";
        }
    }

    public String getPrefix()
    {
        if(this.base != null)
        {
            return this.base.getPrefix();
        }
        else
        {
            return prefix;
        }
    }

    public void setPrefix(String prefix)
    {
        if(this.base != null)
        {
            this.base.setPrefix(prefix);
        }
        else
        {
            this.prefix = prefix;
        }
    }

    @Override
    public String toString()
    {
        return getExpressionString();
    }

    private int findIndexOfStartingBracket(String expression)
    {
        int closeCount = 0;
        for(int i = expression.length() - 1; i > 0; i--)
        {
            if(expression.charAt(i) == '[')
            {
                closeCount--;
                if(closeCount < 0)
                {
                    return i;
                }
            }
            else if(expression.charAt(i) == ']')
            {
                closeCount++;
            }
        }
        return 0;
    }
}
